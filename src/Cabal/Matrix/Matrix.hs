module Cabal.Matrix.Matrix where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Rectangle (Rectangle)
import Cabal.Matrix.Rectangle qualified as Rectangle
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable
import Distribution.Client.Config
import Distribution.Client.GlobalFlags
import Distribution.Client.IndexUtils
import Distribution.Client.Sandbox
import Distribution.Client.Types.SourcePackageDb
import Distribution.Package
import Distribution.Pretty
import Distribution.Solver.Types.PackageIndex qualified as PackageIndex
import Distribution.Verbosity qualified as Verbosity
import Distribution.Version


-- | A build matrix is represented by a rectangle, where properties that we
-- iterate over correspond to columns (first 'Text'), and desired combinations
-- of values of those properties (second 'Text') correspond to rows. Each row is
-- then marked with the collection of options that this combination of choices
-- produces (the 'Flavor').
type Matrix = Rectangle Flavor Text (Maybe Text)

timesMatrix :: Matrix -> Matrix -> Matrix
timesMatrix = Rectangle.productRows (<>)

addMatrix :: Matrix -> Matrix -> Matrix
addMatrix = Rectangle.appendRowsUnioningColumns Nothing

subtractMatrix :: Matrix -> Matrix -> Matrix
subtractMatrix = Rectangle.subtractRowsBySubsetColumns

seqMatrix :: Matrix -> Matrix -> Matrix
seqMatrix m1 m2 = Rectangle.blockDiagonal m1 Nothing Nothing m2

unitMatrix :: Matrix
unitMatrix = Rectangle.unitRow mempty

newtype Compiler = Compiler FilePath
  deriving newtype (Show)

compilersMatrix :: [Compiler] -> Matrix
compilersMatrix compilers = Rectangle.vertical "COMPILER"
  [ ( Flavor
      { unorderedOptions = Set.singleton ("--with-compiler=" <> compiler)
      , orderedOptions = []
      }
    , Just compiler
    )
  | Compiler (Text.pack -> compiler) <- compilers
  ]

data Prefer = PreferOldest | PreferNewest
  deriving stock (Eq, Show)

preferMatrix :: [Prefer] -> Matrix
preferMatrix values = Rectangle.vertical "PREFER"
  [ ( Flavor
      { unorderedOptions = if value == PreferOldest
        then Set.singleton "--prefer-oldest"
        else Set.empty
      , orderedOptions = []
      }
    , Just case value of
      PreferOldest -> "oldest"
      PreferNewest -> "newest"
    )
  | value <- values
  ]

packageVersionMatrix :: PackageName -> [Version] -> Matrix
packageVersionMatrix (Text.pack . unPackageName -> package) versions
  = Rectangle.vertical package
    [ ( Flavor
        { unorderedOptions = Set.singleton
          $ "--constraint=" <> package <> "==" <> version
        , orderedOptions = []
        }
      , Just version
      )
    | version <- Text.pack . prettyShow <$> versions
    ]

customUnorderedOptions :: Text -> [Text] -> Matrix
customUnorderedOptions name options = Rectangle.vertical name
  [ ( Flavor
      { unorderedOptions = Set.fromList options
      , orderedOptions = []
      }
    , Just $ Text.unwords options
    )
  ]

customOrderedOptions :: Text -> [Text] -> Matrix
customOrderedOptions name options = Rectangle.vertical name
  [ ( Flavor
      { unorderedOptions = Set.empty
      , orderedOptions = options
      }
    , Just $ Text.unwords options
    )
  ]

-- | An unevaluated build matrix expression. Matrices can contain exponentially
-- many rows in the worst case, so it makes sense to delay converting into an
-- evaluated representation.
data MatrixExpr
  = TimesExpr MatrixExpr MatrixExpr
  | AddExpr MatrixExpr MatrixExpr
  | SubtractExpr MatrixExpr MatrixExpr
  | SeqExpr MatrixExpr MatrixExpr
  | UnitExpr
  | CompilersExpr [Compiler]
  | PreferExpr [Prefer]
  | PackageVersionExpr PackageName [Either Version VersionRange]
  | CustomUnorderedExpr Text [Text]
  | CustomOrderedExpr Text [Text]

-- | Evaluating the build matrix expression may require access to the source
-- package DB, but if it doesn't, we'd like to avoid loading it.
data EvalM a = EvalPure a | EvalWithDB (SourcePackageDb -> a)
  deriving stock (Functor)

instance Applicative EvalM where
  pure = EvalPure
  EvalPure f <*> EvalPure x = EvalPure (f x)
  EvalPure f <*> EvalWithDB x = EvalWithDB \db -> f (x db)
  EvalWithDB f <*> EvalPure x = EvalWithDB \db -> f db x
  EvalWithDB f <*> EvalWithDB x = EvalWithDB \db -> f db (x db)

evalMatrixExprM :: MatrixExpr -> EvalM Matrix
evalMatrixExprM = \case
  TimesExpr m1 m2
    -> timesMatrix <$> evalMatrixExprM m1 <*> evalMatrixExprM m2
  AddExpr m1 m2
    -> addMatrix <$> evalMatrixExprM m1 <*> evalMatrixExprM m2
  SubtractExpr m1 m2
    -> subtractMatrix <$> evalMatrixExprM m1 <*> evalMatrixExprM m2
  SeqExpr m1 m2
    -> seqMatrix <$> evalMatrixExprM m1 <*> evalMatrixExprM m2
  UnitExpr
    -> EvalPure unitMatrix
  CompilersExpr compilers
    -> EvalPure $ compilersMatrix compilers
  PreferExpr values
    -> EvalPure $ preferMatrix values
  PackageVersionExpr package versions
    -> packageVersionMatrix package . concat <$> for versions \case
      Left version -> pure [version]
      Right range -> EvalWithDB \db -> packageVersion
        <$> PackageIndex.lookupDependency db.packageIndex package range
  CustomUnorderedExpr name values
    -> EvalPure $ customUnorderedOptions name values
  CustomOrderedExpr name values
    -> EvalPure $ customOrderedOptions name values

evalMatrixExpr :: MatrixExpr -> IO Matrix
evalMatrixExpr expr = case evalMatrixExprM expr of
  EvalPure matrix -> pure $! matrix
  EvalWithDB f -> withDB \db -> pure $! f db
  where
    withDB :: (SourcePackageDb -> IO a) -> IO a
    withDB k = do
      config <- loadConfigOrSandboxConfig verbosity defaultGlobalFlags
      withRepoContext verbosity config.savedGlobalFlags \repo -> do
        getSourcePackages verbosity repo >>= k
    verbosity = Verbosity.silent