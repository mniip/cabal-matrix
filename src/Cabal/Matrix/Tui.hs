module Cabal.Matrix.Tui
  ( tuiMain
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Cli
import Cabal.Matrix.Matrix
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
import Data.Primitive
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Options.Applicative hiding ((<|>))


data AppState = AppState
  { headers :: HeaderState
  , headerEditor :: Maybe HeaderEditorState
  , flavorStates :: IntMap FlavorState
  , openedCell :: Maybe (FlavorIndex, OutputState)
  , table :: TableState
  }

initAppState :: Matrix -> (Flavor -> CabalStep -> NonEmpty Text) -> AppState
initAppState matrix mkCmdline = AppState
  { headers = mkHeaderState matrix vertical
  , headerEditor = Nothing
  , flavorStates = IntMap.fromList $ zip [0..]
    $ map (\flavor -> initFlavorState $ tabulateCabalStep' $ mkCmdline flavor)
    $ toList $ Rectangle.rows matrix
  , openedCell = Nothing
  , table = initTableState
  }
  where
    !numColumns = sizeofArray $ Rectangle.columns matrix
    !vertical = arrayFromListN numColumns
      [i * 2 >= numColumns | i <- [0 .. numColumns - 1]]

mkTableMeta :: AppState -> TableMeta
mkTableMeta ast = TableMeta
  { frozenRows = sizeofArray
    $ Rectangle.columns ast.headers.horizontalHeader
  , normalRows = sizeofArray
    $ Rectangle.rows ast.headers.verticalHeader
  , frozenColumns = sizeofArray
    $ Rectangle.columns ast.headers.verticalHeader
  , normalColumns = sizeofArray
    $ Rectangle.rows ast.headers.horizontalHeader
  }

appWidget
  :: DisplayRegion
  -> Matrix
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
        (mkTableMeta ast) table ast.table
      = (ast { headerEditor = Just hes', table = ts' },) $ horizCat
        [ editor
        , charFill defAttr borderNS 1 (height - 1)
        , tbl
        ]
      | (ts', tbl) <- tableWidget (width, height - 1)
        (mkTableMeta ast) table ast.table
      = (ast { table = ts' },) $ tbl

    table = Table
      { frozenRowHeader = indexArray
        $ Rectangle.columns ast.headers.horizontalHeader
      , frozenRowCell = \x y -> Rectangle.indexCell
        ast.headers.horizontalHeader y x
      , frozenColumnHeader = indexArray
        $ Rectangle.columns ast.headers.verticalHeader
      , frozenColumnCell = \x y -> Rectangle.indexCell
        ast.headers.verticalHeader x y
      , normalCell = \x y -> do
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
  :: Matrix -> PerCabalStep Bool -> AppEvent -> AppState -> Maybe AppState
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
  VtyEvent (EvKey KEsc _)
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
    -> Just ast { headers = headers', headerEditor = Just headerEditor' }
  VtyEvent (EvKey (KChar 'x') _)
    -> Just ast { headerEditor = Just initHeaderEditorState }
  VtyEvent (EvKey (KChar ' ') _)
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
    -> Just ast { table = tableHandleEvent (mkTableMeta ast) ev ast.table }

appKeybinds :: AppState -> [(Text, Text)]
appKeybinds ast
  | Just _ <- ast.openedCell
  = [("<Esc>", "back")] <> outputKeybinds
  | Just _ <- ast.headerEditor
  = [("<Esc>", "back")] <> headerEditorKeybinds
  | otherwise
  = (if
      | ast.table.activeSelection == SelectionNormal
      , ast.table.normalSelectionCol < sizeofArray
        (Rectangle.rows ast.headers.horizontalHeader)
      , ast.table.normalSelectionRow < sizeofArray
        (Rectangle.rows ast.headers.verticalHeader)
      , Just _ <- Rectangle.indexCell ast.headers.gridToFlavor
        ast.table.normalSelectionCol ast.table.normalSelectionRow
      -> [("<Space>", "output")]
      | otherwise
      -> [])
    <> [("<Esc>", "quit")]
    <> [("X", "axes")]
    <> tableKeybinds


data AppEvent
  = VtyEvent Event
  | SchedulerEvent SchedulerMessage
  | AppTimerEvent TimerEvent

data TuiOptions = TuiOptions
  { jobs :: Int
  , options :: [Text]
  , targets :: [Text]
  , steps :: PerCabalStep Bool
  , matrixExpr :: MatrixExpr
  , mode :: CabalMode
  }

tui :: TuiOptions -> IO ()
tui TuiOptions{..} = do
  let
    !matrix = evalMatrixExpr matrixExpr
    !flavors = Rectangle.rows matrix

  vty <- mkVty defaultConfig

  queue <- newTBQueueIO 1
  _ <- forkIO $ forever do
    atomically . writeTBQueue queue . VtyEvent =<< vty.nextEvent
  _ <- forkIO $ forever do
    threadDelay 100000
    atomically . writeTBQueue queue . AppTimerEvent $ TimerEvent
  _hdl <- startScheduler SchedulerInput{..}
    (atomically . writeTBQueue queue . SchedulerEvent)

  let
    go !ast = do
      bounds <- vty.outputIface.displayBounds
      let (!ast', !image) = appWidget bounds matrix steps ast
      vty.update $ picForImage image
      ev <- atomically $ readTBQueue queue
      case appHandleEvent matrix steps ev ast' of
        Just ast'' -> go ast''
        Nothing -> vty.shutdown

  go $ initAppState matrix \flavor step -> renderCabalArgs CabalArgs{..}

  -- TODO: should probably kill the processes in _hdl

tuiMain :: IO ()
tuiMain = do
  cliOptions <- execParser cliParser
  options <- case cliOptions of
    CliOptions { matrixExprOrError = Right matrixExpr, .. }
      -> pure TuiOptions{..}
    CliOptions { matrixExprOrError = Left err }
      -> handleParseResult $ Failure $ parserFailure defaultPrefs cliParser
        (ErrorMsg $ Text.unpack err) mempty
  tui options
