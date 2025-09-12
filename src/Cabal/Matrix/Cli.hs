module Cabal.Matrix.Cli
  ( RecordOptions(..)
  , recordParser
  ) where

import Control.Applicative
import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Matrix
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char
import Data.List.Split
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable
import Options.Applicative
import Options.Applicative.Help.Pretty
import Options.Applicative.Types


data RecordOptions = RecordOptions
  { jobs :: Int
  , options :: [Text]
  , targets :: [Text]
  , steps :: PerCabalStep Bool
  , matrixExpr :: MatrixExpr
  , mode :: CabalMode
  }

recordParser :: ParserInfo RecordOptions
recordParser = info (options <**> helper) mempty
  where
    options = RecordOptions
      <$> jobsOption
      <*> optionsOptions
      <*> targetsOptions
      <*> stepsOptions
      <*> frameOptions
      <*> modeOption
    modeOption = flag ProjectBuild InstallLib (long "install-lib"
      <> help "Use cabal install --lib instead of cabal build, allowing \
        \targeting libraries directly from hackage, without a local project")
    jobsOption = option auto (long "jobs" <> short 'j'
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

data MatrixOption
  = OpenParen
  | CloseParen
  | Expr MatrixExpr
  | BinOp (MatrixExpr -> MatrixExpr -> MatrixExpr)

unflatten :: [MatrixOption] -> Maybe MatrixExpr
unflatten input = case runStateT term input of
  Just (output, []) -> Just output
  _ -> Nothing
  where
    term = atom >>= termSuffix
    termSuffix e = ((binOp <*> pure e <*> atom) >>= termSuffix) <|> pure e
    atom = (openParen *> term <* closeParen) <|> expr
    openParen = StateT \case
      OpenParen:xs -> Just ((), xs)
      _ -> Nothing
    closeParen = StateT \case
      CloseParen:xs -> Just ((), xs)
      _ -> Nothing
    expr = StateT \case
      Expr e:xs -> Just (e, xs)
      _ -> Nothing
    binOp = StateT \case
      BinOp b:xs -> Just (b, xs)
      _ -> Nothing

frameOptions :: Parser MatrixExpr
frameOptions = parserOptionGroup "Specifying configurations:" $ fromM do
  options <- oneM $ many frameOption
  maybe (oneM Control.Applicative.empty) pure $ unflatten options
  where
    frameOption = asum
      [ Expr <$> asum
        [ CompilersExpr <$> option readCompilers
          (long "compiler" <> short 'w' <> metavar "COMPILER1,COMPILER2,..."
            <> help "Specify a comma-separated list of compilers")
        , uncurry PackageVersionExpr <$> option readPackage
          (long "package" <> metavar "PACKAGE=VERSION1,VERSION2,..."
            <> help "Specify a comma-separated list of versions of a given \
              \package")
        , PreferExpr <$> option readPrefer
          (long "prefer" <> metavar "[newest],[oldest]"
            <> help "Specify whether to try newest or oldest versions of \
              \packages, or both")
        ]
      , BinOp <$> asum
        [ flag' TimesExpr
          (long "times" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Take the cartesian product of two lists")
        , flag' AddExpr
          (long "add" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Append the two lists, merging common fields")
        , flag' SubtractExpr
          (long "subtract" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Remove from the first list everything that is covered by \
              \the second list")
        , flag' SeqExpr
          (long "seq" <> style (\doc -> "LIST" <+> doc <+> "LIST")
            <> help "Append the two lists, appending columns even if common")
        ]
      , Expr <$> asum
        [ flag' UnitExpr
          (long "default"
            <> help "A single build without any additional options")
        , uncurry CustomUnorderedExpr <$> option readCustom
          (long "custom-options" <> metavar "NAME=--opt1,--opt2,..."
            <> help "A single build with the provided options. NAME is the \
              \field name, which defines how this build is categorized in the \
              \matrix. --add'ing multiple --custom-options with the same NAME \
              \can be used to create a list.")
        , uncurry CustomOrderedExpr <$> option readCustom
          (long "custom-ordered-options" <> metavar "NAME=--opt1,--opt2,..."
            <> help "A single build with the provided options, for options \
              \are order-sensitive. The provided options are appended after \
              \all other options.")
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

readPackage :: ReadM (PackageName, [Version])
readPackage = byEquals
  (PackageName . Text.strip <$> str)
  (commaSeparated (Version . Text.strip <$> str))

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
