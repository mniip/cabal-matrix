module Cabal.Matrix.Record
  ( RunOptions(..)
  , record
  , StepResult(..)
  , FlavorResult(..)
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Cli
import Cabal.Matrix.Matrix
import Cabal.Matrix.ProcessRunner
import Cabal.Matrix.Rectangle qualified as Rectangle
import Cabal.Matrix.Scheduler
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
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
import Data.Text.Encoding qualified as Text
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

collapseOutput :: [(OutputChannel, ByteString)] -> [(OutputChannel, ByteString)]
collapseOutput = map (\grp -> (fst $ NonEmpty.head grp, foldMap snd grp))
  . NonEmpty.groupBy ((==) `on` fst)

record :: Matrix -> RunOptions -> IO [FlavorResult]
record matrix RunOptions{..} = do
  let
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
  pure $ zipWith mkFlavorResult (toList statics) (toList frozenResults)

-- | The ultimate result of having completed a single step of a single flavor.
data StepResult = StepResult
  { cmdline :: NonEmpty Text
  , output :: [(OutputChannel, Text)]
  , exitCode :: ExitCode
  }

data FlavorResult = FlavorResult
  { flavor :: Map Text Text
  , steps :: PerCabalStep (Maybe StepResult)
  }

mkFlavorResult :: StaticFlavorResult -> PerCabalStep StepState -> FlavorResult
mkFlavorResult StaticFlavorResult{..} pcs = FlavorResult
  { flavor
  , steps = tabulateCabalStep' mk
  }
  where
    mk step
      | !state <- indexCabalStep pcs step
      , !cmdline <- indexCabalStep cmdlines step
      = do
        guard state.started
        exitCode <- state.exit
        let
          output = map (fmap Text.decodeUtf8Lenient)
            $ collapseOutput $ reverse state.revOutput
        pure StepResult{..}
