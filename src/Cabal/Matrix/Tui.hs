module Cabal.Matrix.Tui
  ( TuiLiveArgs(..)
  , tuiLive
  , tuiRecording
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Matrix
import Cabal.Matrix.RecordResult
import Cabal.Matrix.Rectangle (Rectangle)
import Cabal.Matrix.Rectangle qualified as Rectangle
import Cabal.Matrix.Scheduler
import Cabal.Matrix.Tui.Common
import Cabal.Matrix.Tui.Flavor
import Cabal.Matrix.Tui.Headers
import Cabal.Matrix.Tui.Table
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Primitive
import Data.Set qualified as Set
import Data.Text (Text)
import Graphics.Vty
import Graphics.Vty.CrossPlatform


data AppState = AppState
  { headers :: HeaderState
  , headerEditor :: Maybe HeaderEditorState
  , flavorStates :: IntMap FlavorState
  , openedCell :: Maybe (FlavorIndex, OutputState)
  , layout :: TableLayout
  , table :: TableState
  }

type TuiMatrix = Rectangle (PerCabalStep (NonEmpty Text)) Text (Maybe Text)

initAppState :: TuiMatrix -> AppState
initAppState matrix = AppState
  { headers
  , headerEditor = Nothing
  , flavorStates = IntMap.fromList
    $ zip [0..] $ map initFlavorState $ toList $ Rectangle.rows matrix
  , openedCell = Nothing
  , layout = mkTableLayout headers
  , table = initTableState
  }
  where
    !numColumns = sizeofArray $ Rectangle.columns matrix
    !vertical = arrayFromListN numColumns
      [i * 2 >= numColumns | i <- [0 .. numColumns - 1]]
    !headers = mkHeaderState matrix vertical

mkTableMeta :: HeaderState -> TableMeta
mkTableMeta headers = TableMeta
  { frozenRows = sizeofArray $ Rectangle.columns headers.horizontalHeader
  , normalRows = sizeofArray $ Rectangle.rows headers.verticalHeader
  , frozenColumns = sizeofArray $ Rectangle.columns headers.verticalHeader
  , normalColumns = sizeofArray $ Rectangle.rows headers.horizontalHeader
  }

mkTableHeaders :: HeaderState -> TableHeaders
mkTableHeaders headers = TableHeaders
  { frozenRowHeader = indexArray $ Rectangle.columns headers.horizontalHeader
  , frozenRowCell = \x y -> Rectangle.indexCell headers.horizontalHeader y x
  , frozenColumnHeader = indexArray $ Rectangle.columns headers.verticalHeader
  , frozenColumnCell = \x y -> Rectangle.indexCell headers.verticalHeader x y
  }

mkTableLayout :: HeaderState -> TableLayout
mkTableLayout headers
  = tableLayout (mkTableMeta headers) (mkTableHeaders headers)

appWidget
  :: DisplayRegion
  -> TuiMatrix
  -> PerCabalStep Bool
  -> AppState
  -> (AppState, Image)
appWidget (width, height) matrix enabledSteps ast
  = (ast', contents <-> keybinds)
  where
    (ast', contents)
      | Just (flavorIndex, os) <- ast.openedCell
      , Just fs <- IntMap.lookup flavorIndex ast.flavorStates
      = (ast,) $ outputWidget (width, height - 1) enabledSteps fs os
      | Just hes <- ast.headerEditor
      , (hes', editor) <- headerEditorWidget (width, height - 1)
        matrix ast.headers hes
      , (ts', tbl) <- tableWidget (width - imageWidth editor - 1, height - 1)
        (mkTableMeta ast.headers) table ast.layout ast.table
      = (ast { headerEditor = Just hes', table = ts' },) $ horizCat
        [ editor
        , charFill defAttr borderNS 1 (height - 1)
        , tbl
        ]
      | (ts', tbl) <- tableWidget (width, height - 1)
        (mkTableMeta ast.headers) table ast.layout ast.table
      = (ast { table = ts' },) $ tbl

    table = TableContents
      { normalCell = \x y -> do
        i <- Rectangle.indexCell ast.headers.gridToFlavor x y
        IntMap.lookup i ast.flavorStates
      }

    blueBg = defAttr `withBackColor` blue
    keybinds = resizeWidthFill blueBg ' ' width $ horizCat $
      intersperse (char blueBg ' ' <|> char blueBg borderNS <|> char blueBg ' ')
      [ text' (blueBg `withForeColor` brightYellow) bind
        <|> text' blueBg (": " <> desc)
      | (bind, desc) <- appKeybinds ast
      ]

appHandleEvent
  :: TuiMatrix
  -> PerCabalStep Bool
  -> AppEvent
  -> AppState
  -> Maybe AppState
appHandleEvent matrix enabledSteps aev ast = case aev of
  SchedulerEvent ev@OnStepStarted{ flavorIndex } -> Just ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  SchedulerEvent ev@OnStepFinished{ flavorIndex } -> Just ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  SchedulerEvent ev@OnOutput{ flavorIndex } -> Just ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  SchedulerEvent OnDone -> Just ast
  AppTimerEvent ev -> Just ast
    { flavorStates = IntMap.map (flavorHandleTimerEvent ev) ast.flavorStates }
  VtyEvent (EvKey (isExitKey -> True) _)
    | Just _ <- ast.openedCell
    -> Just ast { openedCell = Nothing }
    | Just _ <- ast.headerEditor
    -> Just ast { headerEditor = Nothing }
    | Nothing <- ast.headerEditor
    -> Nothing
  VtyEvent ev
    | Just (flavorIndex, os) <- ast.openedCell
    -> Just ast
      { openedCell = Just (flavorIndex, outputHandleEvent enabledSteps ev os) }
    | Just headerEditor <- ast.headerEditor
    , (headerEditor', headers')
      <- headerEditorHandleEvent matrix ev (headerEditor, ast.headers)
    -> Just ast
      { headers = headers'
      , headerEditor = Just headerEditor'
      , layout = mkTableLayout headers'
      }
  VtyEvent (EvKey (KChar 'x') _)
    -> Just ast { headerEditor = Just initHeaderEditorState }
  VtyEvent (EvKey (isEnterKey -> True) _)
    | ast.table.activeSelection == SelectionNormal
    , ast.table.normalSelectionCol < sizeofArray
      (Rectangle.rows ast.headers.horizontalHeader)
    , ast.table.normalSelectionRow < sizeofArray
      (Rectangle.rows ast.headers.verticalHeader)
    , Just flavorIndex <- Rectangle.indexCell ast.headers.gridToFlavor
      ast.table.normalSelectionCol ast.table.normalSelectionRow
    , Just fs <- IntMap.lookup flavorIndex ast.flavorStates
    -> Just ast { openedCell = Just (flavorIndex, initOutputState fs)}
    | otherwise -> Just ast
  VtyEvent ev
    -> Just ast
      { table = tableHandleEvent (mkTableMeta ast.headers) ev ast.table }
  where
    isExitKey = \case
      KEsc -> True
      KChar 'q' -> True
      _ -> False
    isEnterKey = \case
      KChar ' ' -> True
      KEnter -> True
      _ -> False

appKeybinds :: AppState -> [(Text, Text)]
appKeybinds ast
  | Just _ <- ast.openedCell
  = [("<Esc>/Q", "back")] <> outputKeybinds
  | Just _ <- ast.headerEditor
  = [("<Esc>/Q", "back")] <> headerEditorKeybinds
  | otherwise
  = (if
      | ast.table.activeSelection == SelectionNormal
      , ast.table.normalSelectionCol < sizeofArray
        (Rectangle.rows ast.headers.horizontalHeader)
      , ast.table.normalSelectionRow < sizeofArray
        (Rectangle.rows ast.headers.verticalHeader)
      , Just _ <- Rectangle.indexCell ast.headers.gridToFlavor
        ast.table.normalSelectionCol ast.table.normalSelectionRow
      -> [("<Enter>/<Space>", "output")]
      | otherwise
      -> [])
    <> [("<Esc>/Q", "quit")]
    <> [("X", "axes")]
    <> tableKeybinds

data AppEvent
  = VtyEvent Event
  | SchedulerEvent SchedulerMessage
  | AppTimerEvent TimerEvent

tuiMainLoop
  :: TuiMatrix -> AppState -> PerCabalStep Bool -> TBQueue AppEvent -> IO ()
tuiMainLoop tuiMatrix ast0 steps queue = do
  vty <- mkVty defaultConfig

  _ <- forkIO $ forever do
    atomically . writeTBQueue queue . VtyEvent =<< vty.nextEvent

  let
    go !ast = do
      bounds <- vty.outputIface.displayBounds
      let (!ast', !image) = appWidget bounds tuiMatrix steps ast
      vty.update $ picForImage image
      ev <- atomically $ readTBQueue queue
      case appHandleEvent tuiMatrix steps ev ast' of
        Just ast'' -> go ast''
        Nothing -> vty.shutdown

  go ast0

data TuiLiveArgs = TuiLiveArgs
  { jobs :: Int
  , options :: [Text]
  , targets :: [Text]
  , steps :: PerCabalStep Bool
  , matrixExpr :: MatrixExpr
  , mode :: CabalMode
  }

tuiMatrixLive :: TuiLiveArgs -> Matrix -> TuiMatrix
tuiMatrixLive TuiLiveArgs{..}
  = Rectangle.mapRows \flavor -> tabulateCabalStep' \step
    -> renderCabalArgs CabalArgs{..}

schedulerInputLive :: TuiLiveArgs -> Array Flavor -> SchedulerInput
schedulerInputLive TuiLiveArgs{..} flavors = SchedulerInput{..}

tuiLive :: TuiLiveArgs -> IO ()
tuiLive args = do
  !matrix <- evalMatrixExpr args.matrixExpr
  let
    !tuiMatrix = tuiMatrixLive args matrix
    !flavors = Rectangle.rows matrix

  queue <- newTBQueueIO 1
  _ <- forkIO $ forever do
    threadDelay 100000
    atomically . writeTBQueue queue . AppTimerEvent $ TimerEvent
  _hdl <- startScheduler (schedulerInputLive args flavors)
    (atomically . writeTBQueue queue . SchedulerEvent)

  tuiMainLoop tuiMatrix (initAppState tuiMatrix) args.steps queue

  -- TODO: should probably kill the processes in _hdl

tuiMatrixRecording :: RecordResult -> TuiMatrix
tuiMatrixRecording (RecordResult results) = matrix
  where
    !cols = Set.toList $ Set.unions
      [ Map.keysSet result.flavor
      | result <- results
      ]
    !matrix = Rectangle.fromRowMajor cols
      [ ( tabulateCabalStep' \step -> cmdline result step
        , [Map.lookup col result.flavor | col <- cols]
        )
      | result <- results
      ]
    cmdline result = \case
      DryRun -> maybe noCmdline (.cmdline) result.dryRun
      OnlyDownload -> maybe noCmdline (.cmdline) result.onlyDownload
      OnlyDependencies -> maybe noCmdline (.cmdline) result.onlyDependencies
      FullBuild -> maybe noCmdline (.cmdline) result.fullBuild
    noCmdline = pure "" -- TODO: better representation?

addOutputFromRecording :: RecordResult -> AppState -> AppState
addOutputFromRecording (RecordResult results) ast = ast
  { flavorStates = foldl'
    (\im (i, result) -> IntMap.adjust (flavorFromRecording result) i im)
    ast.flavorStates
    (zip [0..] results)
  }

stepsRecording :: RecordResult -> PerCabalStep Bool
stepsRecording (RecordResult results) = tabulateCabalStep' \case
  DryRun -> any (isJust . (.dryRun)) results
  OnlyDownload -> any (isJust . (.onlyDownload)) results
  OnlyDependencies -> any (isJust . (.onlyDependencies)) results
  FullBuild -> any (isJust . (.fullBuild)) results

tuiRecording :: RecordResult -> IO ()
tuiRecording results = do
  let
    !tuiMatrix = tuiMatrixRecording results
    !steps = stepsRecording results

  queue <- newTBQueueIO 1

  tuiMainLoop tuiMatrix
    (addOutputFromRecording results $ initAppState tuiMatrix)
    steps queue
