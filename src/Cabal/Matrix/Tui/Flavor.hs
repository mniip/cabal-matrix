module Cabal.Matrix.Tui.Flavor
  ( FlavorState
  , initFlavorState
  , flavorFromRecording
  , TimerEvent(..)
  , flavorHandleTimerEvent
  , flavorHandleSchedulerEvent
  , OutputState(..)
  , initOutputState
  , outputWidget
  , outputHandleEvent
  , outputKeybinds
  , matrixCellWidth
  , matrixCellHeight
  , cellWidget
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.ProcessRunner
import Cabal.Matrix.Record
import Cabal.Matrix.Scheduler
import Cabal.Matrix.Tui.Common
import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Graphics.Vty
import System.Exit


data StepState = StepState
  { cmdline :: NonEmpty Text
  , started :: Bool
  , revOutput :: [(OutputChannel, ByteString)]
  , lastTickOutputCount :: !Int
  , outputCount :: !Int
  , exit :: Maybe ExitCode
  }

initStepState :: NonEmpty Text -> StepState
initStepState cmdline = StepState
  { cmdline
  , started = False
  , revOutput = []
  , lastTickOutputCount = 0
  , outputCount = 0
  , exit = Nothing
  }

stepFromRecording :: StepResult -> StepState -> StepState
stepFromRecording result ss = ss
  { started = True
  , revOutput = reverse $ second Text.encodeUtf8 <$> result.output
  , exit = Just result.exitCode
  }

statusColor :: StepState -> Color
statusColor ss = case ss.exit of
  _ | not ss.started -> white
  Nothing -> brightYellow
  Just ExitSuccess -> brightGreen
  Just (ExitFailure _) -> brightRed

data ComputedHeader = ComputedHeader
  { height :: Int
  , image :: Image
  }

computeHeader :: DisplayRegion -> StepState -> ComputedHeader
computeHeader (width, _height) ss = ComputedHeader
  { image = vertCat
    [ horizCat $ padToCommonHeight
      [ ( headerBg
        , ' '
        , text' (headerBg `withForeColor` brightYellow) commandText
        )
      , ( headerBg
        , ' '
        , vertCat
          [ resizeWidthFill headerBg ' ' (width - safeWctwidth commandText)
            $ text' headerBg line
          | line <- wrappedCommandText 
          ]
        )
      ]
    , resizeWidthFill headerBg ' ' width
      $ text' (headerBg `withForeColor` brightYellow) "Status: " <|>
        case ss.exit of
          _ | not ss.started -> text' status "Pending"
          Nothing -> text' status "Running"
          Just ExitSuccess -> text' status "Completed successfully"
          Just (ExitFailure code) -> text' status
            $ "Failed with exit code " <> Text.pack (show code)
    , charFill (headerBg `withForeColor` brightYellow) borderEW width 1
    ]
  , height = length wrappedCommandText + 1 + 1
    -- ^ should be equal to @imageHeight image@
  }
  where
    commandText = "Command: "
    headerBg = defAttr `withBackColor` blue
    wrappedCommandText = wrap (width - safeWctwidth commandText)
      $ Text.unwords $ NonEmpty.toList ss.cmdline
    status = headerBg `withForeColor` statusColor ss

data ComputedOutput = ComputedOutput
  { wantedHeight :: Int
  , availableHeight :: Int
  , image :: Image
  }

computeOutput
  :: DisplayRegion
  -> ScrollState
  -> StepState
  -> ComputedOutput
computeOutput (width, height) mScroll ss = ComputedOutput
  { wantedHeight = length wrappedOutputLines
  , availableHeight = height
  , image = resize width height $ vertCat
    [ text' defAttr line
    | line <- wrappedOutputLines
      & case mScroll of
        Nothing -> reverse . take height . reverse
        Just scroll -> take height . drop scroll
    ]
  }
  where
    wrappedOutputLines = wrap width
      $ Text.concat $ Text.decodeUtf8Lenient . snd <$> reverse ss.revOutput

data TimerEvent = TimerEvent

stepHandleSchedulerEvent :: SchedulerMessage -> StepState -> StepState
stepHandleSchedulerEvent ev ss = case ev of
  OnStepStarted{} -> ss
    { started = True
    , exit = Nothing -- in case this is a restart
    }
  OnStepFinished{ exitCode } -> ss { exit = Just exitCode }
  OnOutput{ channel, output } -> ss
    { revOutput = (channel, output):ss.revOutput
    , outputCount = ss.lastTickOutputCount + 1
    }
  OnDone{} -> ss

stepHandleTimerEvent :: TimerEvent -> StepState -> StepState
stepHandleTimerEvent ev ss = case ev of
  TimerEvent -> ss { lastTickOutputCount = ss.outputCount }

type FlavorState = PerCabalStep StepState

initFlavorState :: PerCabalStep (NonEmpty Text) -> FlavorState
initFlavorState = fmap initStepState

flavorFromRecording :: FlavorResult -> FlavorState -> FlavorState
flavorFromRecording result fs = tabulateCabalStep'
  \step -> maybe id stepFromRecording
    (indexCabalStep result.steps step) (indexCabalStep fs step)

flavorHandleSchedulerEvent :: SchedulerMessage -> FlavorState -> FlavorState
flavorHandleSchedulerEvent ev fs = case ev of
  OnStepStarted{ step }
    -> modifyCabalStep step (stepHandleSchedulerEvent ev) fs
  OnStepFinished{ step }
    -> modifyCabalStep step (stepHandleSchedulerEvent ev) fs
  OnOutput{ step }
    -> modifyCabalStep step (stepHandleSchedulerEvent ev) fs
  OnDone{} -> fs

flavorHandleTimerEvent :: TimerEvent -> FlavorState -> FlavorState
flavorHandleTimerEvent ev fs
  = tabulateCabalStep' \step -> stepHandleTimerEvent ev $ indexCabalStep fs step

type ScrollState = Maybe Int
  -- ^ Nothing = scrolled to the bottom. Otherwise, number of word-wrapped
  -- lines to hide from the top.

data OutputState = OutputState
  { selectedStep :: CabalStep
  , scrollState :: ScrollState
  }

initOutputState :: FlavorState -> OutputState
initOutputState fs = OutputState
  { selectedStep = fromMaybe FullBuild $ find
    (\step -> indexCabalStep fs step
      & \ss -> ss.started && ss.exit /= Just ExitSuccess)
    [minBound..maxBound]
  , scrollState = Nothing
  }

data ComputedWidget = ComputedWidget  
  { wantedOutputHeight :: Int
  , availableOutputHeight :: Int
  , image :: Image
  }

computeOutputWidget
  :: DisplayRegion
  -> PerCabalStep Bool
  -> FlavorState
  -> OutputState
  -> ComputedWidget
computeOutputWidget (width, height) enabledSteps fs os = ComputedWidget
  { wantedOutputHeight = output.wantedHeight
  , availableOutputHeight = output.availableHeight
  , image = tabSwitcher <-> header.image <-> output.image
  }
  where
    stepName = \case
      DryRun -> "Planning"
      OnlyDownload -> "Download"
      OnlyDependencies -> "Dependencies"
      FullBuild -> "Build"
    tabSwitcher = horizCat
      [ char defAttr ' ' <|> text'
        (if step == os.selectedStep
          then defAttr `withBackColor` blue
          else defAttr `withBackColor` brightBlack)
        (if ss'.started && isNothing ss'.exit
          then stepName step <> " " <> outputSpinner ss'
          else stepName step)
      | step <- [minBound..maxBound]
      , indexCabalStep enabledSteps step || step == os.selectedStep
      , let ss' = indexCabalStep fs step
      ]
    tabSwitcherHeight = 1 -- should be equal to @imageHeight tabSwitcher@

    ss = indexCabalStep fs os.selectedStep
    header = computeHeader (width, height - tabSwitcherHeight) ss
    output = computeOutput (width, height - tabSwitcherHeight - header.height)
      os.scrollState ss

outputWidget
  :: DisplayRegion -> PerCabalStep Bool -> FlavorState -> OutputState -> Image
outputWidget displayRegion enabledSteps fs os
  = (computeOutputWidget displayRegion enabledSteps fs os).image

outputSpinner :: StepState -> Text
outputSpinner ss = Text.singleton $ "|/-\\" !! (ss.outputCount `mod` 4)

outputHandleEvent
  :: DisplayRegion
  -> PerCabalStep Bool
  -> Event
  -> FlavorState
  -> OutputState
  -> OutputState
outputHandleEvent displayRegion enabledSteps ev fs os = case ev of
  -- Make sure to do something sensible if all steps are disabled
  EvKey KLeft _ -> os
    { selectedStep = fromMaybe os.selectedStep $ listToMaybe
      [ step
      | step <- reverse [minBound..maxBound]
      , indexCabalStep enabledSteps step
      , step < os.selectedStep
      ]
    , scrollState = Nothing
    }
  EvKey KRight _ -> os
    { selectedStep = fromMaybe os.selectedStep $ listToMaybe
      [ step
      | step <- [minBound..maxBound]
      , indexCabalStep enabledSteps step
      , step > os.selectedStep
      ]
    , scrollState = Nothing
    }
  EvKey KUp _ -> os
    { scrollState = performScroll (-1)
    }
  EvKey KDown _ -> os
    { scrollState = performScroll 1
    }
  EvKey KPageUp _ -> os
    { scrollState = performScroll (-output.availableOutputHeight)
    }
  EvKey KPageDown _ -> os
    { scrollState = performScroll output.availableOutputHeight
    }
  _ -> os
  where
    output = computeOutputWidget displayRegion enabledSteps fs os
    performScroll by
      = mfilter (<= output.wantedOutputHeight - output.availableOutputHeight)
      $ Just
      $ max 0
      $ (+ by)
      $ fromMaybe (output.wantedOutputHeight - output.availableOutputHeight)
      $ os.scrollState

outputKeybinds :: [(Text, Text)]
outputKeybinds =
  [ (Text.pack [triangleW, triangleE], "switch build steps")
  ]

matrixCellWidth :: Int
matrixCellWidth = 10

matrixCellHeight :: Int
matrixCellHeight = 1

cellWidget :: Bool -> Maybe FlavorState -> Image
cellWidget focused Nothing = charFill
  (if focused then defAttr `withBackColor` brightBlack else defAttr)
  ' ' matrixCellWidth matrixCellHeight
cellWidget focused (Just pcs) = resizeWidthFill attr ' ' matrixCellWidth if
  | Just ExitSuccess <- pcs.fullBuild.exit
  -> go FullBuild "build ok"
  | Just (ExitFailure _) <- pcs.fullBuild.exit
  -> go FullBuild "build fail"
  | pcs.fullBuild.started
  -> go FullBuild $ "build " <> outputSpinner pcs.fullBuild

  | Just ExitSuccess <- pcs.onlyDependencies.exit
  -> go OnlyDependencies "deps ok"
  | Just (ExitFailure _) <- pcs.onlyDependencies.exit
  -> go OnlyDependencies "deps fail"
  | pcs.onlyDependencies.started
  -> go OnlyDependencies $ "deps " <> outputSpinner pcs.onlyDependencies

  | Just ExitSuccess <- pcs.onlyDownload.exit
  -> go OnlyDownload "DL ok"
  | Just (ExitFailure _) <- pcs.onlyDownload.exit
  -> go OnlyDownload "DL fail"
  | pcs.onlyDownload.started
  -> go OnlyDownload $ "DL " <> outputSpinner pcs.onlyDownload

  | Just ExitSuccess <- pcs.dryRun.exit
  -> go DryRun "plan ok"
  | Just (ExitFailure _) <- pcs.dryRun.exit
  -> go DryRun "no plan"
  | pcs.dryRun.started
  -> go DryRun $ "plan " <> outputSpinner pcs.dryRun

  | otherwise
  -> text' attr "..."
  where
    attr = if focused then defAttr `withBackColor` blue else defAttr
    go step = text' $ attr `withForeColor` if
      -- A failure to --dry-run, that is, to solve package constraints is often
      -- not considered a failure. Rather it indicates that the package bounds
      -- are set up correctly.
      | DryRun <- step
      , Just (ExitFailure _) <- (indexCabalStep pcs step).exit
      -> cyan
      | otherwise
      -> statusColor (indexCabalStep pcs step)
