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
import Cabal.Matrix.RecordResult
import Cabal.Matrix.Scheduler
import Cabal.Matrix.Tui.Common
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
  , lastTickOutputCount :: Int
  , outputCount :: Int
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
  , exit = Just case result.exitCode of
    0 -> ExitSuccess
    code -> ExitFailure code
  }

statusColor :: StepState -> Color
statusColor ss
  | not ss.started = white
  | Nothing <- ss.exit = brightYellow
  | Just ExitSuccess <- ss.exit = brightGreen
  | Just (ExitFailure _) <- ss.exit = brightRed

stepOutputWidget :: DisplayRegion -> StepState -> Image
stepOutputWidget (width, height) ss = header <-> output
  where
    commandText = "Command: "
    headerBg = defAttr `withBackColor` blue
    status = headerBg `withForeColor` statusColor ss
    header = vertCat
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
            | line <- wrap (width - safeWctwidth commandText)
              $ Text.unwords $ NonEmpty.toList ss.cmdline
            ]
          )
        ]
      , resizeWidthFill headerBg ' ' width
        $ text' (headerBg `withForeColor` brightYellow) "Status: " <|> if
          | not ss.started -> text' status "Pending"
          | Nothing <- ss.exit -> text' status "Running"
          | Just ExitSuccess <- ss.exit -> text' status
            "Completed successfully"
          | Just (ExitFailure code) <- ss.exit -> text' status
            $ "Failed with exit code " <> Text.pack (show code)
      , charFill (headerBg `withForeColor` brightYellow) borderEW width 1
      ]
    output = resize width (height - imageHeight header) $ vertCat
      [ text' defAttr line
      | line <- reverse . take (height - imageHeight header) . reverse
        $ wrap width
        $ Text.concat $ Text.decodeUtf8Lenient . snd <$> reverse ss.revOutput
      ]

data TimerEvent = TimerEvent

stepHandleSchedulerEvent :: SchedulerMessage -> StepState -> StepState
stepHandleSchedulerEvent ev ss = case ev of
  OnStepStarted{} -> ss { started = True }
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
flavorFromRecording result fs = PerCabalStep
  { dryRun
    = maybe id stepFromRecording result.dryRun fs.dryRun
  , onlyDownload
    = maybe id stepFromRecording result.onlyDownload fs.onlyDownload
  , onlyDependencies
    = maybe id stepFromRecording result.onlyDependencies fs.onlyDependencies
  , fullBuild
    = maybe id stepFromRecording result.fullBuild fs.fullBuild
  }

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
flavorHandleTimerEvent ev = fmap (stepHandleTimerEvent ev)

newtype OutputState = OutputState
  { selectedStep :: CabalStep
  }

initOutputState :: FlavorState -> OutputState
initOutputState fs = OutputState
  { selectedStep = fromMaybe FullBuild $ find
    (\step -> indexCabalStep fs step
      & \ss -> ss.started && ss.exit /= Just ExitSuccess)
    [minBound..maxBound]
  }

outputWidget
  :: DisplayRegion -> PerCabalStep Bool -> FlavorState -> OutputState -> Image
outputWidget (width, height) enabledSteps fs os = tabSwitcher <-> output
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
        (if ss.started && isNothing ss.exit
          then stepName step <> " " <> outputSpinner ss
          else stepName step)
      | step <- [minBound..maxBound]
      , indexCabalStep enabledSteps step || step == os.selectedStep
      , let ss = indexCabalStep fs step
      ]
    output = stepOutputWidget (width, height - imageHeight tabSwitcher)
      (indexCabalStep fs os.selectedStep)

outputSpinner :: StepState -> Text
outputSpinner ss = Text.singleton $ "|/-\\" !! (ss.outputCount `mod` 4)

outputHandleEvent
  :: PerCabalStep Bool -> Event -> OutputState -> OutputState
outputHandleEvent enabledSteps ev os = case ev of
  -- Make sure to do something sensible if all steps are disabled
  EvKey KLeft _ -> os
    { selectedStep = fromMaybe os.selectedStep $ listToMaybe
      [ step
      | step <- reverse [minBound..maxBound]
      , indexCabalStep enabledSteps step
      , step < os.selectedStep
      ]
    }
  EvKey KRight _ -> os
    { selectedStep = fromMaybe os.selectedStep $ listToMaybe
      [ step
      | step <- [minBound..maxBound]
      , indexCabalStep enabledSteps step
      , step > os.selectedStep
      ]
    }
  _ -> os

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
      -> green
      | otherwise
      -> statusColor (indexCabalStep pcs step)
