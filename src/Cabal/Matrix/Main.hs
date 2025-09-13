module Cabal.Matrix.Main
  ( main
  ) where

import Cabal.Matrix.Cli
import Cabal.Matrix.Record
import Cabal.Matrix.RecordResult
import Cabal.Matrix.Tui
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Options.Applicative

main :: IO ()
main = do
  cliOptions <- execParser cliParser
  case cliOptions of
    Builds { matrixExprOrError = Left err }
      -> handleParseResult $ Failure $ parserFailure defaultPrefs cliParser
        (ErrorMsg $ Text.unpack err) mempty
    Builds { matrixExprOrError = Right matrixExpr, recordTo = Nothing, .. }
      -> tuiLive TuiLiveArgs{..}
    Builds { matrixExprOrError = Right matrixExpr, recordTo = Just file, .. }
      -> do
        results <- record RecordArgs{..}
        Aeson.encodeFile @RecordResult file results
    Replay file
      -> do
        results <- Aeson.eitherDecodeFileStrict @RecordResult file >>= \case
          Left err -> fail err
          Right results -> pure results
        tuiRecording results