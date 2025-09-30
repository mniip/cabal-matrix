module Cabal.Matrix.Main
  ( main
  ) where

import Cabal.Matrix.Cli
import Cabal.Matrix.Files
import Cabal.Matrix.Matrix
import Cabal.Matrix.Record
import Cabal.Matrix.Tui
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative

main :: IO ()
main = do
  cliOptions <- execParser cliParser
  case cliOptions of
    Left err -> showCliError err
    Right ExprToExpr{ exprSource, outFile } -> do
      expr <- getExpr exprSource
      expr' <- resolveMatrixExpr expr
      writeExprFile outFile expr'
    Right ExprToMatrix{ exprSource, outFile } -> do
      expr <- getExpr exprSource
      matrix <- evalMatrixExpr expr
      writeMatrixFile outFile matrix
    Right ExprToRecord{ exprSource, runParams, outFile } -> do
      expr <- getExpr exprSource
      matrix <- evalMatrixExpr expr
      results <- record matrix runParams
      writeRecordFile outFile results
    Right ExprToTui{ exprSource, runParams } -> do
      expr <- getExpr exprSource
      matrix <- evalMatrixExpr expr
      tuiLive matrix runParams
    Right MatrixToRecord{ matrixFile, runParams, outFile } -> do
      matrix <- readMatrixFile matrixFile
      results <- record matrix runParams
      writeRecordFile outFile results
    Right MatrixToTui{ matrixFile, runParams } -> do
      matrix <- readMatrixFile matrixFile
      tuiLive matrix runParams
    Right RecordToTui{ recordFile } -> do
      results <- readRecordFile recordFile
      tuiRecording results

getExpr :: ExprSource -> IO MatrixExpr
getExpr = \case
  ExprArgs (Left err) -> showCliError err
  ExprArgs (Right expr) -> pure expr
  ExprFile file -> readExprFile file

showCliError :: Text -> IO a
showCliError err = handleParseResult
  $ Failure $ parserFailure defaultPrefs cliParser
    (ErrorMsg $ Text.unpack err) mempty
