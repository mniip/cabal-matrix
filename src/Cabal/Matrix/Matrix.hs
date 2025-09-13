module Cabal.Matrix.Matrix where

import Cabal.Matrix.CabalArgs
import Cabal.Matrix.Rectangle (Rectangle)
import Cabal.Matrix.Rectangle qualified as Rectangle
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Distribution.Package
import Distribution.Pretty
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
  | PackageVersionExpr PackageName [Version]
  | CustomUnorderedExpr Text [Text]
  | CustomOrderedExpr Text [Text]

evalMatrixExpr :: MatrixExpr -> Matrix
evalMatrixExpr = \case
  TimesExpr m1 m2 -> timesMatrix (evalMatrixExpr m1) (evalMatrixExpr m2)
  AddExpr m1 m2 -> addMatrix (evalMatrixExpr m1) (evalMatrixExpr m2)
  SubtractExpr m1 m2 -> subtractMatrix (evalMatrixExpr m1) (evalMatrixExpr m2)
  SeqExpr m1 m2 -> seqMatrix (evalMatrixExpr m1) (evalMatrixExpr m2)
  UnitExpr -> unitMatrix
  CompilersExpr compilers -> compilersMatrix compilers
  PreferExpr values -> preferMatrix values
  PackageVersionExpr package versions -> packageVersionMatrix package versions
  CustomUnorderedExpr name values -> customUnorderedOptions name values
  CustomOrderedExpr name values -> customOrderedOptions name values


