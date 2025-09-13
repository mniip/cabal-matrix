module Cabal.Matrix.Record
  ( RecordOptions(..)
  , recordMain
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Cli
import Cabal.Matrix.Matrix
import Cabal.Matrix.ProcessRunner
import Cabal.Matrix.RecordResult
import Cabal.Matrix.Rectangle qualified as Rectangle
import Cabal.Matrix.Scheduler
import Control.Concurrent
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString8
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Primitive
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative
import System.Exit


data StaticFlavorResult = StaticFlavorResult
  { flavor :: Map Text Text
  , cmdlines :: PerCabalStep (NonEmpty Text)
  }

data StepState = StepState
  { started :: Bool
  , revOutput :: [(OutputChannel, ByteString)]
  , exit :: Maybe ExitCode
  }

mkFlavorResult :: StaticFlavorResult -> PerCabalStep StepState -> FlavorResult
mkFlavorResult StaticFlavorResult{..} pcs = FlavorResult
  { flavor
  , dryRun = mk DryRun
  , onlyDownload = mk OnlyDownload
  , onlyDependencies = mk OnlyDependencies
  , fullBuild = mk FullBuild
  }
  where
    mk step
      | !state <- indexCabalStep pcs step
      , !cmdline <- indexCabalStep cmdlines step
      = do
        guard state.started
        exitCode <- exitToInt <$> state.exit
        let
          output = map (fmap Text.decodeUtf8Lenient)
            $ collapseOutput $ reverse state.revOutput
        pure StepResult{..}
    exitToInt = \case
      ExitFailure i -> i
      ExitSuccess -> 0

collapseOutput :: [(OutputChannel, ByteString)] -> [(OutputChannel, ByteString)]
collapseOutput = map (\grp -> (fst $ NonEmpty.head grp, foldMap snd grp))
  . NonEmpty.groupBy ((==) `on` fst)

data RecordOptions = RecordOptions
  { jobs :: Int
  , options :: [Text]
  , targets :: [Text]
  , steps :: PerCabalStep Bool
  , matrixExpr :: MatrixExpr
  , mode :: CabalMode
  }

record :: RecordOptions -> IO RecordResult
record RecordOptions{..} = do
  let
    !matrix = evalMatrixExpr matrixExpr
    !flavors = Rectangle.rows matrix
    !statics = arrayFromListN (sizeofArray flavors)
      [ StaticFlavorResult
        { flavor = Map.fromList $ mapMaybe sequenceA pairs
        , cmdlines = tabulateCabalStep' \step -> renderCabalArgs CabalArgs{..}
        }
      | (flavor, pairs) <- Rectangle.toRowMajor matrix
      ]
    schedulerInput = SchedulerInput{..}

  results <- flip traverseArrayP flavors
    \_ -> sequenceA $ tabulateCabalStep' \_ -> newIORef StepState
      { started = False
      , revOutput = []
      , exit = Nothing
      }
  doneVar <- newEmptyMVar
  _ <- startScheduler schedulerInput
      \case
        OnDone -> putMVar doneVar ()
        OnStepStarted{ flavorIndex, step } -> atomicModifyIORef'
          (indexCabalStep (indexArray results flavorIndex) step)
          \state -> (state { started = True }, ())
        OnStepFinished{ flavorIndex, step, exitCode } -> atomicModifyIORef'
          (indexCabalStep (indexArray results flavorIndex) step)
          \state -> (state { exit = Just exitCode }, ())
        OnOutput{ flavorIndex, step, channel, output } -> atomicModifyIORef'
          (indexCabalStep (indexArray results flavorIndex) step)
          \state
            -> (state { revOutput = (channel, output):state.revOutput }, ())

  takeMVar doneVar
  frozenResults <- traverseArrayP (traverse readIORef) results
  pure $ RecordResult
    $ zipWith mkFlavorResult (toList statics) (toList frozenResults)

recordMain :: IO ()
recordMain = do
  cliOptions <- execParser cliParser
  options <- case cliOptions of
    CliOptions { matrixExprOrError = Right matrixExpr, .. }
      -> pure RecordOptions{..}
    CliOptions { matrixExprOrError = Left err }
      -> handleParseResult $ Failure $ parserFailure defaultPrefs cliParser
        (ErrorMsg $ Text.unpack err) mempty
  result <- record options
  LazyByteString8.putStrLn $ Aeson.encode result
