-- | Execute multiple build steps on multiple 'Flavor's of a build, with live
-- progress and output updates.
--
-- The scheduler maintains a job queue of flavors (not steps), so that when it
-- starts work on a flavor, it goes through all of its steps. The scheduler can
-- be told to reprioritize a flavor that hasn't been started yet.
--
-- The scheduler is given an 'Array' of 'Flavor's, and subsequent communication
-- refers to flavors from this array by their index.
module Cabal.Matrix.Scheduler
  ( SchedulerConfig(..)
  , mkCabalArgs
  , FlavorIndex
  , startScheduler
  , SchedulerMessage(..)
  , SchedulerHandle
  , signalScheduler
  , SchedulerSignal(..)
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.ProcessRunner
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import System.Directory
import System.Exit


data SchedulerConfig = SchedulerConfig
  { jobs :: Int
    -- ^ Number of flavors that could be building in parallel at a time.
  , options :: [Text]
    -- ^ Options to use in all builds.
  , targets :: [Text]
  , cabalExecutable :: FilePath
  , mode :: CabalMode
  , steps :: PerCabalStep Bool
    -- ^ Which build steps to run or skip.
  }

-- | Index into 'flavors' of 'SchedulerInput'
type FlavorIndex = Int

data SchedulerMessage
  = OnStepStarted
    { flavorIndex :: FlavorIndex
    , step :: CabalStep
    } -- ^ Either the first message for this flavor, or sequenced after
      -- 'OnStepFinished' for this flavor.
  | OnStepFinished
    { flavorIndex :: FlavorIndex
    , step :: CabalStep
    , exitCode :: ExitCode
    } -- ^ Always sequenced after 'OnOutput' for this flavor.
  | OnOutput
    { flavorIndex :: FlavorIndex
    , step :: CabalStep
    , channel :: OutputChannel
    , output :: ByteString
    } -- ^ Sequenced after 'OnStepStarted' for this flavor.
  | OnDone -- ^ Sequenced after all events.
  deriving stock (Show)

data SchedulerSignal
  = InterruptFlavor
    { flavorIndex :: FlavorIndex
    } -- ^ Try to interrupt the current step (if any) with SIGINT and don't
      -- start subsequent steps (if any).
  | TerminateFlavor
    { flavorIndex :: FlavorIndex
    } -- ^ Try to interrupt the current step (if any) with SIGTERM and don't
      -- start subsequent steps (if any).
  | PrioritizeFlavor
    { flavorIndex :: FlavorIndex
    } -- ^ Move the given flavor the front of the priority queue, if it hasn't
      -- been started yet

newtype SchedulerHandle = SchedulerHandle (MVar SchedulerState)

data SchedulerState = SchedulerState
  { processes :: Map FlavorIndex ProcessHandle
  , stopRequested :: Set FlavorIndex
  , queue :: [FlavorIndex]
    -- ^ INVARIANT: disjoint from 'stopRequested' and from keys of 'processes'
  }

mkCabalArgs :: SchedulerConfig -> CabalStep -> Flavor -> CabalArgs
mkCabalArgs input step flavor = CabalArgs
  { cabalExecutable = input.cabalExecutable
  , step
  , mode = input.mode
  , options = input.options
  , targets = input.targets
  , flavor
  }

startScheduler
  :: SchedulerConfig
  -> Array Flavor
  -> (SchedulerMessage -> IO ())
  -> IO SchedulerHandle
startScheduler input flavors cb = do
  sem <- newQSem input.jobs
  mvar <- newMVar SchedulerState
    { processes = Map.empty
    , stopRequested = Set.empty
    , queue = [0 .. sizeofArray flavors - 1]
    }
  let
    waitForNext :: IO ()
    waitForNext = do
      waitQSem sem
      done <- modifyMVar mvar \state -> case state.queue of
        [] -> do
          doneWithFlavor
          pure (state, True)
        flavorIndex:queue' -> do
          mProcess <- startSteps flavorIndex
            (filter (indexCabalStep input.steps) [minBound..maxBound])
          pure
            ( state
              { processes
                = Map.update (\_ -> mProcess) flavorIndex state.processes
              , queue = queue'
              }
            , False
            )

      if done
      then waitForDone
      else waitForNext

    doneWithFlavor :: IO ()
    doneWithFlavor = signalQSem sem

    startSteps :: FlavorIndex -> [CabalStep] -> IO (Maybe ProcessHandle)
    startSteps flavorIndex = \case
      [] -> Nothing <$ doneWithFlavor
      step:nextSteps -> Just <$> do
        stdoutClosed <- newEmptyMVar
        stderrClosed <- newEmptyMVar
        cb OnStepStarted { flavorIndex, step }
        let args = mkCabalArgs input step (indexArray flavors flavorIndex)
        for_ (environmentFilePath args) $ void . try @_ @IOError . removeFile
        startProcess (renderCabalArgs args)
          (reactStep flavorIndex step nextSteps stdoutClosed stderrClosed)

    reactStep
      :: FlavorIndex
      -> CabalStep
      -> [CabalStep]
      -> MVar ()
      -> MVar ()
      -> ProcessMessage
      -> IO ()
    reactStep flavorIndex step nextSteps stdoutClosed stderrClosed = \case
      OnProcessOutput channel output -> cb OnOutput
        { flavorIndex
        , step
        , channel
        , output
        }
      OnChannelClosed Stdout -> putMVar stdoutClosed ()
      OnChannelClosed Stderr -> putMVar stderrClosed ()
      OnProcessExit exitCode -> do
        -- Synchronize OnStepFinished to be sequenced after OnOutput.
        -- Perhaps ProcessRunner should do this.
        takeMVar stdoutClosed
        takeMVar stderrClosed
        cb OnStepFinished { flavorIndex, step, exitCode }
        case exitCode of
          ExitFailure _ -> doneWithFlavor
          ExitSuccess -> modifyMVar_ mvar \state
            -- It's possible that an interrupt signal has been received after
            -- the process has already exited (successfully), but before we got
            -- the 'OnProcessExit' message. So we must check this flag to see if
            -- we shouldn't start subsequent steps.
            -> if flavorIndex `Set.member` state.stopRequested
              then do
                doneWithFlavor
                pure state
                  { processes = Map.delete flavorIndex state.processes
                  , stopRequested = Set.delete flavorIndex state.stopRequested
                  }
              else do
                mProcess <- startSteps flavorIndex nextSteps
                pure state
                  { processes
                    = Map.update (\_ -> mProcess) flavorIndex state.processes
                  }

    waitForDone :: IO ()
    waitForDone = do
      -- Once the queue has become empty, wait for all running jobs to finish
      replicateM_ input.jobs $ waitQSem sem
      cb OnDone

  _ <- forkIO waitForNext
  pure $ SchedulerHandle mvar

signalScheduler :: SchedulerHandle -> SchedulerSignal -> IO ()
signalScheduler (SchedulerHandle mvar) = \case
  InterruptFlavor{ flavorIndex } -> modifyMVar_ mvar \state -> if
    | flavorIndex `elem` state.queue
    -> pure state { queue = delete flavorIndex state.queue }
    | Just processHdl <- Map.lookup flavorIndex state.processes
    -> do
      signalProcess processHdl SignalInterrupt
      pure state { stopRequested = Set.insert flavorIndex state.stopRequested }
    | otherwise
    -> pure state
  TerminateFlavor{ flavorIndex } -> modifyMVar_ mvar \state -> if
    | flavorIndex `elem` state.queue
    -> pure state { queue = delete flavorIndex state.queue }
    | Just processHdl <- Map.lookup flavorIndex state.processes
    -> do
      signalProcess processHdl SignalTerminate
      pure state { stopRequested = Set.insert flavorIndex state.stopRequested }
    | otherwise
    -> pure state
  PrioritizeFlavor{ flavorIndex } -> modifyMVar_ mvar \state -> if
    | flavorIndex `elem` state.queue
    -> pure state { queue = flavorIndex : delete flavorIndex state.queue }
      -- ^ TODO: leak? we accumulate 'delete' thunks?
    | otherwise
    -> pure state
