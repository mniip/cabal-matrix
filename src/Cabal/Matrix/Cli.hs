module Cabal.Matrix.Cli
  ( CliOptions(..)
  , getSchedulerConfig
  , ExprSource(..)
  , RunOptions(..)
  , cliParser
  ) where

import Control.Applicative
import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Matrix
import Cabal.Matrix.Scheduler
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable
import Distribution.Package
import Distribution.Parsec
import Distribution.Version
import Options.Applicative
import Options.Applicative.Help.Pretty
import Options.Applicative.Types
import System.Environment


data ExprSource
  = ExprArgs (Either Text MatrixExpr)
  | ExprFile FilePath

data RunOptions = RunOptions
  { jobs :: Int
  , cabalOverride :: Maybe FilePath
  , steps :: PerCabalStep Bool
  , options :: [Text]
  , targets :: [Text]
  , mode :: CabalMode
  }

getSchedulerConfig :: RunOptions -> IO SchedulerConfig
getSchedulerConfig RunOptions{..} = do
  cabalExecutable <- case cabalOverride of
    Just override -> pure override
    Nothing -> fromMaybe "cabal" <$> lookupEnv "CABAL"
  pure SchedulerConfig{..}

data CliOptions
  = ExprToExpr
    { exprSource :: ExprSource
    , outFile :: FilePath
    }
  | ExprToMatrix
    { exprSource :: ExprSource
    , outFile :: FilePath
    }
  | ExprToRecord
    { exprSource :: ExprSource
    , runParams :: RunOptions
    , outFile :: FilePath
    }
  | ExprToTui
    { exprSource :: ExprSource
    , runParams :: RunOptions
    }
  | MatrixToRecord
    { matrixFile :: FilePath
    , runParams :: RunOptions
    , outFile :: FilePath
    }
  | MatrixToTui
    { matrixFile :: FilePath
    , runParams :: RunOptions
    }
  | RecordToTui
    { recordFile :: FilePath
    }

data Source
  = FromExpr ExprSource
  | FromMatrix FilePath
  | FromRecord FilePath

data Target
  = ToExpr FilePath
  | ToMatrix FilePath
  | ToRecord FilePath
  | ToTui

cliParser :: ParserInfo (Either Text CliOptions)
cliParser = info (optionsAsCommand <|> options) mempty
  where
    -- Cabal external commands will invoke us as "cabal-matrix matrix $ARGS",
    -- see https://github.com/haskell/cabal/issues/10275 . Using an
    -- optparse-applicative 'command' we can swallow this "matrix" exactly when
    -- it's the first argument -- otherwise we interpret "matrix" as a target.
    -- 'internal' makes it hidden from all help texts.
    optionsAsCommand = subparser
      (internal <> command "matrix" (info options mempty))

    options = mkOptions
      <$> sourceOption
      <*> optional runOptions
      <*> targetOption
      <**> helper
    sourceOption = (FromExpr <$> exprSourceOption)
      <|> (FromMatrix <$> fromMatrixOption)
      <|> (FromRecord <$> fromRecordOption)
    exprSourceOption = (ExprArgs <$> frameOptions)
      <|> (ExprFile <$> exprFileOption)
    targetOption = (ToExpr <$> toExprOption)
      <|> (ToMatrix <$> toMatrixOption)
      <|> (ToRecord <$> toRecordOption)
      <|> (pure ToTui)

    mkOptions source mRunParams target = case (source, target) of
      (FromExpr exprSource, ToExpr outFile)
        -> Right ExprToExpr{..}
      (FromExpr exprSource, ToMatrix outFile)
        -> Right ExprToMatrix{..}
      (FromExpr exprSource, ToRecord outFile) -> case mRunParams of
        Just runParams -> Right ExprToRecord{..}
        Nothing -> noRunOptions
      (FromExpr exprSource, ToTui) -> case mRunParams of
        Just runParams -> Right ExprToTui{..}
        Nothing -> noRunOptions
      (FromMatrix matrixFile, ToRecord outFile) -> case mRunParams of
        Just runParams -> Right MatrixToRecord{..}
        Nothing -> noRunOptions
      (FromMatrix matrixFile, ToTui) -> case mRunParams of
        Just runParams -> Right MatrixToTui{..}
        Nothing -> noRunOptions
      (FromRecord recordFile, ToTui)
        -> Right RecordToTui{..}

      (FromMatrix _, ToExpr _) -> Left
        "--from-matrix cannot be used with --to-expr"
      (FromMatrix _, ToMatrix _) -> Left
        "--from-matrix cannot be used with --to-matrix"
      (FromRecord _, ToExpr _) -> Left
        "--from-record cannot be used with --to-expr"
      (FromRecord _, ToMatrix _) -> Left
        "--from-record cannot be used with --to-matrix"
      (FromRecord _, ToRecord _) -> Left
        "--from-record cannot be used with --to-record"

    exprFileOption = filesGroup
      $ option str (long "from-expr" <> metavar "FILE"
        <> help "Read the expression from FILE previously created by --to-expr")
    toExprOption = filesGroup
      $ option str (long "to-expr" <> metavar "FILE"
        <> help "Instead of running the TUI, resolve the versions in the given \
          \expression and save it to FILE")
    fromMatrixOption = filesGroup
      $ option str (long "from-matrix" <> metavar "FILE"
        <> help "Load the build matrix from FILE previously created by \
          \--to-matrix")
    toMatrixOption = filesGroup
      $ option str (long "to-matrix" <> metavar "FILE"
        <> help "Instead of running the TUI, compute the build matrix and save \
          \it to FILE")
    fromRecordOption = filesGroup
      $ option str (long "from-record" <> metavar "FILE"
        <> help "View the results from FILE previously collected by \
          \--to-record")
    toRecordOption = filesGroup
      $ option str (long "to-record" <> metavar "FILE"
        <> help "Instead of running the TUI, run the builds and collect their \
          \results into FILE")

    noRunOptions = Left "-j|--jobs N is required when not using --from-record, \
      \--to-expr, or --to-matrix"
    filesGroup = parserOptionGroup "Working with files:"

runOptions :: Parser RunOptions
runOptions = parserOptionGroup "Running Cabal:" $ RunOptions
  <$> jobsOption
  <*> cabalExecutableOption
  <*> stepsOptions
  <*> optionsOptions
  <*> targetsOptions
  <*> modeOption
  where
    modeOption = flag ProjectBuild InstallLib (long "install-lib"
      <> help "Use cabal install --lib instead of cabal build, allowing \
        \targeting libraries directly from hackage, without a local project")
    jobsOption = option auto (long "jobs" <> short 'j' <> metavar "N"
      <> help "How many instances of cabal to run concurrently")
    optionsOptions = many $ option str (long "option" <> metavar "--OPTION"
      <> help "Pass an option to cabal in all configurations")
    targetsOptions = many $ argument str (metavar "TARGET"
      <> help "Targets to tell cabal to build")
    stepsOptions = PerCabalStep
      <$> flag True False (long "no-dry-run"
        <> help "Skip the --dry-run build step, where only a plan is created")
      <*> flag True False (long "no-only-download"
        <> help "Skip the --only-download step, where a plan is created and \
          \packages are only downloaded")
      <*> flag True False (long "no-only-dependencies"
        <> help "Skip the --only-dependencies step, where a plan is created \
          \but only the dependencies are built")
      <*> flag True False (long "no-full-build"
        <> help "Skip the normal build, where the selected targets are fully \
          \built")
    cabalExecutableOption = optional $ option auto (metavar "PATH"
      <> help "Use the specified cabal executable. Defaults to $CABAL or \
        \whichever \"cabal\" exists in $PATH.")

data MatrixOption
  = OpenParen
  | CloseParen
  | Expr Text MatrixExpr
  | BinOp Text (MatrixExpr -> MatrixExpr -> MatrixExpr)

unflatten :: [MatrixOption] -> Either Text MatrixExpr
unflatten input = case runStateT (term False) input of
  Left err -> Left err
  Right (output, []) -> Right output
  Right (_, CloseParen:_) -> Left "More closing parens than open"
  Right (_, opt:_) -> Left $ "Unexpected " <> describe opt <> " at end of input"
  where
    term paren = atom >>= termSuffix paren
    termSuffix paren e = StateT \case
      BinOp _ b:xs -> do
        (e', xs') <- runStateT atom xs
        runStateT (termSuffix paren (b e e')) xs'
      xs@(CloseParen:_) -> pure (e, xs)
      xs@[] -> pure (e, xs)
      opt:_ -> Left $ "Expected " <> describeBinOp
        <> (if paren then ", " <> describe CloseParen <> ", " else " ")
        <> "or end of input after list expression, got " <> describe opt
    atom = StateT \case
      OpenParen:xs -> do
        (e, xs') <- runStateT (term True) xs
        case xs' of
          CloseParen:xs'' -> pure (e, xs'')
          [] -> Left "More open parens than closing parens"
          opt:_ -> Left
            $ "Unexpected " <> describe opt <> " after list expression"
      Expr _ e:xs -> pure (e, xs)
      opt:_ -> Left $ "Expecting " <> describeExpr <> " or "
        <> describe OpenParen <> ", got " <> describe opt
      [] -> Left $ "Expecting " <> describeExpr <> " or "
        <> describe OpenParen <> ", got end of input"

    describe = \case
      OpenParen -> "open paren (--[)"
      CloseParen -> "closing paren (--])"
      Expr name _ -> "list option (" <> name <> ")"
      BinOp name _ -> "binary operator option (" <> name <> ")"
    describeExpr = "list option (--compiler, --package, etc)"
    describeBinOp = "binary operator option (--times, --add, etc)"

frameOptions :: Parser (Either Text MatrixExpr)
frameOptions = parserOptionGroup "Specifying configurations:"
  $ unflatten <$> many frameOption
  where
    frameOption = asum
      [ asum
        [ Expr "--compiler" . CompilersExpr <$> option readCompilers
          (long "compiler" <> short 'w' <> metavar "COMPILER1,COMPILER2,..."
            <> help "Specify a comma-separated list of compilers")
        , Expr "--package" . uncurry PackageVersionExpr <$> option readPackage
          (long "package" <> metavar "PACKAGE[=VERSION1,VERSION2,...]"
            <> help "Specify a comma-separated list of versions of a given \
              \package. Each version can instead be a version range such as \
              \'>=1.0 && <2.0'. If only the package name is specified, all \
              \its versions are tried.")
        , Expr "--prefer" . PreferExpr <$> option readPrefer
          (long "prefer" <> metavar "[newest],[oldest]"
            <> help "Specify whether to try newest or oldest versions of \
              \packages, or both")
        ]
      , asum
        [ flag' (BinOp "--times" TimesExpr)
          (long "times" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Take the cartesian product of two lists")
        , flag' (BinOp "--add" AddExpr)
          (long "add" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Append the two lists, merging common fields")
        , flag' (BinOp "--subtract" SubtractExpr)
          (long "subtract" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Remove from the first list everything that is covered by \
              \the second list")
        , flag' (BinOp "--seq" SeqExpr)
          (long "seq" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Append the two lists, appending columns even if common")
        ]
      , asum
        [ flag' (Expr "--default" UnitExpr)
          (long "default"
            <> help "A single build without any additional options")
        , Expr "--custom-options" . uncurry CustomUnorderedExpr
          <$> option readCustom
          (long "custom-options" <> metavar "NAME=--opt1,--opt2,..."
            <> help "A single build with all of the provided options. NAME is \
              \the field name, which defines how this build is categorized in \
              \the matrix. --add'ing multiple --custom-options with the same \
              \NAME can be used to create a list.")
        , Expr "--custom-ordered-options" . uncurry CustomOrderedExpr
          <$> option readCustom
          (long "custom-ordered-options" <> metavar "NAME=--opt1,--opt2,..."
            <> help "A single build with the provided options, for options \
              \that are order-sensitive. The provided options are appended \
              \after all other options.")
        ]
      , flag' OpenParen
        (long "[" <> style (\_ -> "--[ LIST --]")
          <> help "Operations are evaluated from left to right \
            \(left-associative). Brackets can be used to correct the order of \
            \operations")
      , flag' CloseParen (long "]" <> hidden)
      ]

readCompilers :: ReadM [Compiler]
readCompilers
  = map Compiler . filter (not . null) <$> commaSeparated (strip <$> str)

readPrefer :: ReadM [Prefer]
readPrefer = commaSeparated $ maybeReader \(map toLower . strip -> s) -> if
  | s `elem` ["newest", "newer", "new"] -> Just PreferNewest
  | s `elem` ["oldest", "older", "old"] -> Just PreferOldest
  | otherwise -> Nothing

readPackage :: ReadM (PackageName, VersionExpr)
readPackage = do
  s <- readerAsk
  if '=' `elem` s
  then byEquals
    (mkPackageName . strip <$> str)
    (SomeVersions <$> commaSeparated versionOrRange)
  else ((, AllVersions) . mkPackageName . strip <$> str)
  where
    versionOrRange = ReadM $ ReaderT \s -> case eitherParsec @Version s of
      Right ver -> pure $ Left ver
      Left err1 -> case eitherParsec @VersionRange s of
        Right range -> pure $ Right range
        Left err2 -> throwE $ ErrorMsg $ unlines
          ["Expected a version or a version range: ", err1, err2]

readCustom :: ReadM (Text, [Text])
readCustom = byEquals
  (Text.strip <$> str)
  (filter (not . Text.null) <$> commaSeparated (Text.strip <$> str))

byEquals :: ReadM a -> ReadM b -> ReadM (a, b)
byEquals (ReadM (ReaderT pk)) (ReadM (ReaderT pv)) = ReadM $ ReaderT \s -> if
  | (prefix, _:suffix) <- break (=='=') s -> (,) <$> pk prefix <*> pv suffix
  | otherwise -> throwE $ ErrorMsg "Expected key=value pair"

commaSeparated :: ReadM a -> ReadM [a]
commaSeparated (ReadM (ReaderT p)) = ReadM $ ReaderT \s -> for (splitOn "," s) p

strip :: String -> String
strip = Text.unpack . Text.strip . Text.pack
