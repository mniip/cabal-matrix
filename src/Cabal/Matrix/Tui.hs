module Cabal.Matrix.Tui
  ( tuiLive
  , tuiRecording
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Cli
import Cabal.Matrix.Matrix
import Cabal.Matrix.Record
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

appHandleSchedulerMessage
  :: SchedulerMessage
  -> AppState
  -> AppState
appHandleSchedulerMessage ev ast = case ev of
  OnStepStarted{ flavorIndex } -> ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  OnStepFinished{ flavorIndex } -> ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  OnOutput{ flavorIndex } -> ast
    { flavorStates = IntMap.adjust (flavorHandleSchedulerEvent ev)
      flavorIndex ast.flavorStates
    }
  OnDone -> ast

appHandleEvent
  :: TuiMatrix
  -> PerCabalStep Bool
  -> Maybe SchedulerHandle
  -> AppEvent
  -> AppState
  -> IO (Maybe AppState)
appHandleEvent matrix enabledSteps mScheduler aev ast = case aev of
  AppTimerEvent ev -> pure $ Just ast
    { flavorStates = IntMap.map (flavorHandleTimerEvent ev) ast.flavorStates }
  VtyEvent (EvKey (isExitKey -> True) _)
    | Just _ <- ast.openedCell
    -> pure $ Just ast { openedCell = Nothing }
    | Just _ <- ast.headerEditor
    -> pure $ Just ast { headerEditor = Nothing }
    | Nothing <- ast.headerEditor
    -> pure Nothing
  VtyEvent ev
    | Just (flavorIndex, os) <- ast.openedCell
    -> case ev of
      EvKey (KChar 'c') (elem MCtrl -> True) -> do
        for_ mScheduler $ flip signalScheduler InterruptFlavor { flavorIndex }
        pure $ Just ast
      EvKey (KChar '\\') (elem MCtrl -> True) -> do
        for_ mScheduler $ flip signalScheduler TerminateFlavor { flavorIndex }
        pure $ Just ast
      EvKey (KChar 'p') _ -> do
        for_ mScheduler $ flip signalScheduler PrioritizeFlavor { flavorIndex }
        pure $ Just ast
      EvKey (KChar 'r') _ -> do
        for_ mScheduler $ flip signalScheduler RestartFlavor { flavorIndex }
        pure $ Just ast
      _ -> pure $ Just ast
        { openedCell = Just (flavorIndex, outputHandleEvent enabledSteps ev os)
        }
    | Just headerEditor <- ast.headerEditor
    , (headerEditor', headers')
      <- headerEditorHandleEvent matrix ev (headerEditor, ast.headers)
    -> pure $ Just ast
      { headers = headers'
      , headerEditor = Just headerEditor'
      , layout = mkTableLayout headers'
      }
  VtyEvent (EvKey (KChar 'x') _)
    -> pure $ Just ast { headerEditor = Just initHeaderEditorState }
  VtyEvent (EvKey (isEnterKey -> True) _)
    | Just flavorIndex <- astSelectedCell ast
    , Just fs <- IntMap.lookup flavorIndex ast.flavorStates
    -> pure $ Just ast { openedCell = Just (flavorIndex, initOutputState fs) }
    | otherwise -> pure $ Just ast
  VtyEvent (EvKey (KChar 'c') (elem MCtrl -> True))
    | Just flavorIndex <- astSelectedCell ast
    -> do
      for_ mScheduler $ flip signalScheduler InterruptFlavor { flavorIndex }
      pure $ Just ast
  VtyEvent (EvKey (KChar '\\') (elem MCtrl -> True))
    | Just flavorIndex <- astSelectedCell ast
    -> do
      for_ mScheduler $ flip signalScheduler TerminateFlavor { flavorIndex }
      pure $ Just ast
  VtyEvent (EvKey (KChar 'p') _)
    | Just flavorIndex <- astSelectedCell ast
    -> do
      for_ mScheduler $ flip signalScheduler PrioritizeFlavor { flavorIndex }
      pure $ Just ast
  VtyEvent (EvKey (KChar 'r') _)
    | Just flavorIndex <- astSelectedCell ast
    -> do
      for_ mScheduler $ flip signalScheduler RestartFlavor { flavorIndex }
      pure $ Just ast
  VtyEvent ev
    -> pure $ Just ast
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

astSelectedCell :: AppState -> Maybe FlavorIndex
astSelectedCell ast = do
  guard $ ast.table.activeSelection == SelectionNormal
  guard $ ast.table.normalSelectionCol < sizeofArray
    (Rectangle.rows ast.headers.horizontalHeader)
  guard $ ast.table.normalSelectionRow < sizeofArray
    (Rectangle.rows ast.headers.verticalHeader)
  Rectangle.indexCell ast.headers.gridToFlavor
    ast.table.normalSelectionCol ast.table.normalSelectionRow

appKeybinds :: AppState -> [(Text, Text)]
appKeybinds ast
  | Just _ <- ast.openedCell
  = [("<Esc>/Q", "back")] <> outputKeybinds <>
    [ ("^C", "interrupt")
    , ("^\\", "terminate")
    , ("P", "prioritize")
    , ("R", "restart")
    ]
  | Just _ <- ast.headerEditor
  = [("<Esc>/Q", "back")] <> headerEditorKeybinds
  | otherwise
  = (if isJust (astSelectedCell ast)
      then
        [ ("<Enter>/<Space>", "output")
        , ("^C", "interrupt")
        , ("^\\", "terminate")
        , ("P", "prioritize")
        , ("R", "restart")
        ]
      else [])
    <> [("<Esc>/Q", "quit")]
    <> [("X", "axes")]
    <> tableKeybinds

data AppEvent
  = VtyEvent Event
  | AppTimerEvent TimerEvent

tuiMainLoop
  :: TuiMatrix
  -> AppState
  -> PerCabalStep Bool
  -> TBQueue (Either SchedulerMessage AppEvent)
  -> Maybe SchedulerHandle
  -> IO ()
tuiMainLoop tuiMatrix ast0 steps queue mScheduler = do
  vty <- mkVty defaultConfig

  _ <- forkIO $ forever do
    atomically . writeTBQueue queue . Right . VtyEvent =<< vty.nextEvent

  let
    goDisplay !ast = do
      bounds <- vty.outputIface.displayBounds
      let (!ast', !image) = appWidget bounds tuiMatrix steps ast
      vty.update $ picForImage image
      go ast'

    go !ast = atomically (readTBQueue queue) >>= \case
      Left msg -> go (appHandleSchedulerMessage msg ast)
      Right ev -> appHandleEvent tuiMatrix steps mScheduler ev ast >>= \case
        Just ast' -> goDisplay ast'
        Nothing -> vty.shutdown

  goDisplay ast0

tuiMatrixLive :: SchedulerConfig -> Matrix -> TuiMatrix
tuiMatrixLive schedulerConfig
  = Rectangle.mapRows \flavor -> tabulateCabalStep' \step
    -> renderCabalArgs $ mkCabalArgs schedulerConfig step flavor

tuiLive :: Matrix -> RunOptions -> IO ()
tuiLive matrix options = do
  schedulerConfig <- getSchedulerConfig options
  let
    !tuiMatrix = tuiMatrixLive schedulerConfig matrix
    !flavors = Rectangle.rows matrix

  queue <- newTBQueueIO 1
  _ <- forkIO $ forever do
    threadDelay 100000
    atomically . writeTBQueue queue . Right . AppTimerEvent $ TimerEvent
  scheduler <- startScheduler schedulerConfig flavors
    (atomically . writeTBQueue queue . Left)

  tuiMainLoop tuiMatrix (initAppState tuiMatrix) options.steps queue
    (Just scheduler)

  -- TODO: should probably kill the processes in _hdl

tuiMatrixRecording :: [FlavorResult] -> TuiMatrix
tuiMatrixRecording results = matrix
  where
    !cols = Set.toList $ Set.unions
      [ Map.keysSet result.flavor
      | result <- results
      ]
    !matrix = Rectangle.fromRowMajor cols
      [ ( tabulateCabalStep' \step
          -> maybe noCmdline (.cmdline) $ indexCabalStep result.steps step
        , [Map.lookup col result.flavor | col <- cols]
        )
      | result <- results
      ]
    noCmdline = pure "" -- TODO: better representation?

addOutputFromRecording :: [FlavorResult] -> AppState -> AppState
addOutputFromRecording results ast = ast
  { flavorStates = foldl'
    (\im (i, result) -> IntMap.adjust (flavorFromRecording result) i im)
    ast.flavorStates
    (zip [0..] results)
  }

stepsRecording :: [FlavorResult] -> PerCabalStep Bool
stepsRecording results = tabulateCabalStep' \step
  -> any (\res -> isJust $ indexCabalStep res.steps step) results

tuiRecording :: [FlavorResult] -> IO ()
tuiRecording results = do
  let
    !tuiMatrix = tuiMatrixRecording results
    !steps = stepsRecording results

  queue <- newTBQueueIO 1

  tuiMainLoop tuiMatrix
    (addOutputFromRecording results $ initAppState tuiMatrix)
    steps queue Nothing
