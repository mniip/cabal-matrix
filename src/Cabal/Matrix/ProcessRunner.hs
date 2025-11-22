-- | Run a single process and provide a live view of its output and termination.
module Cabal.Matrix.ProcessRunner
  ( startProcess
  , ProcessMessage(..)
  , OutputChannel(..)
  , ProcessHandle
  , signalProcess
  , ProcessSignal(..)
  ) where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import System.Exit
import System.IO
import System.Process qualified as Process


data OutputChannel = Stdout | Stderr
  deriving stock (Eq, Show, Generic)

instance ToJSON OutputChannel where
  toJSON = \case
    Stdout -> "stdout"
    Stderr -> "stderr"

instance FromJSON OutputChannel where
  parseJSON = Aeson.withText "OutputChannel" \case
    "stdout" -> pure Stdout
    "stderr" -> pure Stderr
    v -> fail $ "Unexpected value: " <> show v


data ProcessMessage
  = OnProcessOutput OutputChannel ByteString
    -- ^ Note: Always sequenced before 'OnChannelClosed', but may be sequenced
    -- after 'OnProcessExit'.
  | OnChannelClosed OutputChannel
    -- ^ Note: may be sequenced after 'OnProcessExit'.
  | OnProcessExit ExitCode

data ProcessSignal
  = SignalInterrupt
    -- ^ Attempt to interrupt the process with @SIGINT@.
  | SignalTerminate
    -- ^ Attempt to interrupt the process with @SIGTERM@.

newtype ProcessHandle = ProcessHandle Process.ProcessHandle

-- | Start a process with the given commandline arguments in the background.
-- The provided callback will be called (from other threads) when things happen
-- to the process.
--
-- Note: if the current program should be compiled with @-threaded@, see
-- 'Process.waitForProcess'.
--
-- A process can be considered finished and its resources cleaned up after
-- receiving all three of @'OnChannelClosed' 'Stdout'@,
-- @'OnChannelClosed' 'Stderr'@, and 'OnProcessExit'.
startProcess :: NonEmpty Text -> (ProcessMessage -> IO ()) -> IO ProcessHandle
startProcess cmdline cb = mask_ do
  (_mIn, mOut, mErr, processHdl) <- Process.createProcess
    case Text.unpack <$> cmdline of
      cmd :| args -> (Process.proc cmd args)
        { Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        , Process.create_group = True
        }
  _ <- forkIOWithUnmask \unmask -> do
    for_ mOut (\out -> unmask $ readLoop out Stdout)
      `finally` cb (OnChannelClosed Stdout)
  _ <- forkIOWithUnmask \unmask -> do
    for_ mErr (\err -> unmask $ readLoop err Stderr)
      `finally` cb (OnChannelClosed Stderr)
  _ <- forkIOWithUnmask \unmask -> do
    unmask (Process.waitForProcess processHdl >>= cb . OnProcessExit)
      `onException` Process.terminateProcess processHdl
  pure $ ProcessHandle processHdl
  where
    readLoop :: Handle -> OutputChannel -> IO ()
    readLoop hdl chan = do
      bs <- ByteString.hGetSome hdl 0x10000
      unless (ByteString.null bs) do
        cb $ OnProcessOutput chan bs
        readLoop hdl chan

-- | Send a signal to the process.
signalProcess :: ProcessHandle -> ProcessSignal -> IO ()
signalProcess (ProcessHandle processHdl) = \case
  SignalInterrupt -> Process.interruptProcessGroupOf processHdl
  SignalTerminate -> Process.terminateProcess processHdl


