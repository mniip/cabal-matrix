module Cabal.Matrix.Files
  ( writeExprFile
  , readExprFile
  , writeMatrixFile
  , readMatrixFile
  , writeRecordFile
  , readRecordFile
  ) where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Matrix
import Cabal.Matrix.Record
import Cabal.Matrix.Rectangle qualified as Rectangle
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Types
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import System.Exit


newtype PreferJSON = PreferJSON Prefer

instance ToJSON PreferJSON where
  toJSON = \case
    PreferJSON PreferOldest -> "old"
    PreferJSON PreferNewest -> "new"

instance FromJSON PreferJSON where
  parseJSON = withText "Prefer" \case
    "old" -> pure $ PreferJSON PreferOldest
    "new" -> pure $ PreferJSON PreferNewest
    _ -> fail "Expected \"old\" or \"new\""

newtype VersionJSON = VersionJSON (Either Version VersionRange)

instance ToJSON VersionJSON where
  toJSON = \case
    VersionJSON (Left version) -> toJSON $ prettyShow version
    VersionJSON (Right range) -> toJSON $ prettyShow range

instance FromJSON VersionJSON where
  parseJSON = withText "Version" \(Text.unpack -> s)
    -> VersionJSON <$> case eitherParsec @Version s of
      Right ver -> pure $ Left ver
      Left err1 -> case eitherParsec @VersionRange s of
        Right range -> pure $ Right range
        Left err2 -> fail $ unlines
          ["Expected a version or a version range: ", err1, err2]

newtype ConstraintJSON = ConstraintJSON Constraint

instance ToJSON ConstraintJSON where
  toJSON (ConstraintJSON constr) = object
    [ "package" .= unPackageName constr.package
    , "versions" .= prettyShow constr.versions
    ]

instance FromJSON ConstraintJSON where
  parseJSON = withObject "Constraint" \o -> do
    package <- mkPackageName <$> o .: "package"
    versions <- explicitParseField parseVersion o "versions"
    pure $ ConstraintJSON Constraint{..}
    where
      parseVersion = withText "Version" \txt -> do
        either fail pure $ eitherParsec @VersionRange $ Text.unpack txt

newtype MatrixExprJSON = MatrixExprJSON MatrixExpr

instance ToJSON MatrixExprJSON where
  toJSON (MatrixExprJSON e) = go e
    where
      go = \case
        -- Format TimesExpr (TimesExpr ... e2) e1 as {"times":[...,e2,e1]}
        TimesExpr x y -> goTimes x [MatrixExprJSON y]
        AddExpr x y -> goAdd x [MatrixExprJSON y]
        SubtractExpr x y -> goSubtract x [MatrixExprJSON y]
        SeqExpr x y -> goSeq x [MatrixExprJSON y]
        UnitExpr -> "unit"
        CompilersExpr compilers -> object
          [ "compilers" .= toJSON
            (compilers <&> \(Compiler compiler) -> compiler)
          ]
        PreferExpr values -> object
          [ "prefer" .= toJSON (PreferJSON <$> values)
          ]
        PackageVersionExpr packageName versions -> object
          [ "package" .= unPackageName packageName
          , "versions" .= case versions of
            AllVersions -> Null
            SomeVersions verList -> toJSON (VersionJSON <$> verList)
          ]
        CustomUnorderedExpr key options -> object
          [ "custom_unordered" .= key
          , "options" .= toJSON options
          ]
        CustomOrderedExpr key options -> object
          [ "custom_ordered" .= key
          , "options" .= toJSON options
          ]
        ConstraintsExpr disj -> object
          [ "constraints" .=
            [ [ConstraintJSON constr | constr <- Set.toList conj.unConjunction]
            | conj <- Set.toList disj.unDisjunction
            ]
          ]
      goTimes (TimesExpr x y) ys = goTimes x (MatrixExprJSON y : ys)
      goTimes y xs = object
        [ "times" .= toJSON (MatrixExprJSON y : xs)
        ]
      goAdd (AddExpr x y) ys = goAdd x (MatrixExprJSON y : ys)
      goAdd y xs = object
        [ "add" .= toJSON (MatrixExprJSON y : xs)
        ]
      goSubtract (SubtractExpr x y) ys = goSubtract x (MatrixExprJSON y : ys)
      goSubtract y xs = object
        [ "subtract" .= toJSON (MatrixExprJSON y : xs)
        ]
      goSeq (SeqExpr x y) ys = goSeq x (MatrixExprJSON y : ys)
      goSeq y xs = object
        [ "seq" .= toJSON (MatrixExprJSON y : xs)
        ]

instance FromJSON MatrixExprJSON where
  parseJSON (String "unit") = pure $ MatrixExprJSON UnitExpr
  parseJSON (String _) = fail "Expected string to be \"unit\""
  parseJSON (Object o) = do
    parses <- sequenceA
      [ (o .:? "times") <&> fmap @Maybe
        (foldl1 @NonEmpty TimesExpr . fmap \(MatrixExprJSON x) -> x)
      , (o .:? "add") <&> fmap @Maybe
        (foldl1 @NonEmpty AddExpr . fmap \(MatrixExprJSON x) -> x)
      , (o .:? "subtract") <&> fmap @Maybe
        (foldl1 @NonEmpty SubtractExpr . fmap \(MatrixExprJSON x) -> x)
      , (o .:? "seq") <&> fmap @Maybe
        (foldl1 @NonEmpty SeqExpr . fmap \(MatrixExprJSON x) -> x)
      , (o .:? "compilers") <&> fmap @Maybe (CompilersExpr . map Compiler)
      , (o .:? "prefer") <&> fmap @Maybe
        (PreferExpr . map \(PreferJSON value) -> value)
      , o .:? "package" >>= \case
        Nothing -> pure Nothing
        Just package -> do
          versions <- o .:? "versions" <&> \case
            Nothing -> AllVersions
            Just versions -> SomeVersions
              $ versions <&> \(VersionJSON version) -> version
          pure $ Just $ PackageVersionExpr (mkPackageName package) versions
      , o .:? "custom_unordered" >>= \case
        Nothing -> pure Nothing
        Just key -> o .:? "options" >>= \case
          Just options -> pure $ Just $ CustomUnorderedExpr key options
          Nothing -> fail
            "Expected \"custom_unordered\" to be accompanied by \"options\""
      , o .:? "custom_ordered" >>= \case
        Nothing -> pure Nothing
        Just key -> o .:? "options" >>= \case
          Just options -> pure $ Just $ CustomOrderedExpr key options
          Nothing -> fail
            "Expected \"custom_ordered\" to be accompanied by \"options\""
      , (o .:? "constraints") <&> fmap @Maybe \disj
        -> ConstraintsExpr $ Disjunction $ Set.fromList
          [ Conjunction $ Set.fromList
            [constr | ConstraintJSON constr <- conj]
          | conj <- disj
          ]
      ]
    case catMaybes parses of
      [x] -> pure $ MatrixExprJSON x
      _ -> fail "Expected exactly one of: \"times\", \"add\", \"subtract\", \
        \\"seq\", \"compilers\", \"prefer\", \"package\", \
        \\"custom_unordered\", \"custom_ordered\", \"constraints\""
  parseJSON v = typeMismatch "Object or String" v

writeExprFile :: FilePath -> MatrixExpr -> IO ()
writeExprFile file = Aeson.encodeFile @MatrixExprJSON file . MatrixExprJSON

readExprFile :: FilePath -> IO MatrixExpr
readExprFile file = Aeson.eitherDecodeFileStrict @MatrixExprJSON file >>= \case
  Left err -> fail err
  Right (MatrixExprJSON expr) -> pure expr

newtype MatrixRowJSON = MatrixRowJSON (Flavor, Map Text Text)

instance ToJSON MatrixRowJSON where
  toJSON (MatrixRowJSON (flavor, row)) = object
    [ "constraints" .=
      [ [ [ConstraintJSON constr | constr <- Set.toList conj.unConjunction]
        | conj <- Set.toList disj.unDisjunction
        ]
      | disj <- Set.toList flavor.constraints.unConjunction
      ]
    , "ordered_options" .= flavor.orderedOptions
    , "unordered_options" .= flavor.unorderedOptions
    , "flavor" .= row
    ]

instance FromJSON MatrixRowJSON where
  parseJSON = withObject "MatrixRow" \o -> do
    mConstraints <- o .:? "constraints"
    let
      constraints = Conjunction $ Set.fromList
        [ Disjunction $ Set.fromList
          [ Conjunction $ Set.fromList
            [constr | ConstraintJSON constr <- conj]
          | conj <- disj
          ]
        | disj <- fromMaybe [] mConstraints
        ]
    orderedOptions <- o .: "ordered_options"
    unorderedOptions <- o .: "unordered_options"
    flavor <- o .: "flavor"
    pure $ MatrixRowJSON (Flavor{..}, flavor)

newtype MatrixJSON = MatrixJSON Matrix

instance ToJSON MatrixJSON where
  toJSON (MatrixJSON matrix) = toJSON
    [ MatrixRowJSON (flavor, Map.fromList $ mapMaybe sequenceA row)
    | (flavor, row) <- Rectangle.toRowMajor matrix
    ]

instance FromJSON MatrixJSON where
  parseJSON v = MatrixJSON <$> do
    !rows <- parseJSON v
    let
      !cols = Set.toList $ Set.unions
        [ Map.keysSet row
        | MatrixRowJSON (_, row) <- rows
        ]
      !matrix = Rectangle.fromRowMajor cols
        [ ( flavor
          , [Map.lookup col row | col <- cols]
          )
        | MatrixRowJSON (flavor, row) <- rows
        ]
    pure matrix

writeMatrixFile :: FilePath -> Matrix -> IO ()
writeMatrixFile file = Aeson.encodeFile @MatrixJSON file . MatrixJSON

readMatrixFile :: FilePath -> IO Matrix
readMatrixFile file = Aeson.eitherDecodeFileStrict @MatrixJSON file >>= \case
  Left err -> fail err
  Right (MatrixJSON matrix) -> pure matrix

newtype StepResultJSON = StepResultJSON StepResult

instance ToJSON StepResultJSON where
  toJSON (StepResultJSON result) = object
    [ "cmdline" .= result.cmdline
    , "output" .= result.output
    , "exit_code" .= case result.exitCode of
      ExitSuccess -> 0
      ExitFailure code -> code
    ]
  -- Have to manually define toEncoding to force field ordering.
  toEncoding (StepResultJSON result) = Encoding.pairs $ mconcat
    [ Encoding.pair "cmdline" $ toEncoding result.cmdline
    , Encoding.pair "output" $ toEncoding result.output
    , Encoding.pair "exit_code" $ toEncoding case result.exitCode of
      ExitSuccess -> 0
      ExitFailure code -> code
    ]

instance FromJSON StepResultJSON where
  parseJSON = Aeson.withObject "StepResult" \o -> do
    cmdline <- o Aeson..: "cmdline"
    output <- o Aeson..: "output"
    exitCode <- o Aeson..: "exit_code" <&> \case
      0 -> ExitSuccess
      code -> ExitFailure code
    pure $ StepResultJSON StepResult{..}

newtype FlavorResultJSON = FlavorResultJSON FlavorResult

instance ToJSON FlavorResultJSON where
  toJSON (FlavorResultJSON result) = object $ catMaybes
    [ ("flavor" .=) <$> Just result.flavor
    , ("dry_run" .=) . StepResultJSON
      <$> result.steps.dryRun
    , ("only_download" .=) . StepResultJSON
      <$> result.steps.onlyDownload
    , ("only_dependencies" .=) . StepResultJSON
      <$> result.steps.onlyDependencies
    , ("full_build" .=) . StepResultJSON
      <$> result.steps.fullBuild
    ]
  -- Have to manually define toEncoding to force field ordering.
  toEncoding (FlavorResultJSON result) = Encoding.pairs $ fold
    [ Encoding.pair "flavor" $ toEncoding result.flavor
    , foldMap (Encoding.pair "dry_run" . toEncoding)
      $ StepResultJSON <$> result.steps.dryRun
    , foldMap (Encoding.pair "only_download" . toEncoding)
      $ StepResultJSON <$> result.steps.onlyDownload
    , foldMap (Encoding.pair "only_dependencies" . toEncoding)
      $ StepResultJSON <$> result.steps.onlyDependencies
    , foldMap (Encoding.pair "full_build" . toEncoding)
      $ StepResultJSON <$> result.steps.fullBuild
    ]

instance FromJSON FlavorResultJSON where
  parseJSON = withObject "FlavorResult" \o -> do
    flavor <- o .: "flavor"
    dryRun <- o .:? "dry_run"
    onlyDownload <- o .:? "only_download"
    onlyDependencies <- o .:? "only_dependencies"
    fullBuild <- o .:? "full_build"
    pure $ FlavorResultJSON FlavorResult
      { flavor
      , steps = PerCabalStep{..}
        <&> fmap @Maybe \(StepResultJSON result) -> result
      }

writeRecordFile :: FilePath -> [FlavorResult] -> IO ()
writeRecordFile file = Aeson.encodeFile @[FlavorResultJSON] file
  . map FlavorResultJSON

readRecordFile :: FilePath -> IO [FlavorResult]
readRecordFile file = Aeson.eitherDecodeFileStrict @[FlavorResultJSON] file
  >>= \case
    Left err -> fail err
    Right results -> pure $ results <&> \(FlavorResultJSON result) -> result
