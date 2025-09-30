-- | Converting high-level build matrix concepts into @cabal@ commandlines.
module Cabal.Matrix.CabalArgs
  ( CabalArgs(..)
  , CabalStep(..)
  , PerCabalStep(..)
  , indexCabalStep
  , tabulateCabalStep'
  , modifyCabalStep
  , setCabalStep
  , CabalMode(..)
  , Flavor(..)
  , renderCabalArgs
  , environmentFilePath
  ) where

import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import System.FilePath


-- | Cabal allows interrupting the build process at multiple points, which is
-- useful to know which stage the build failed at.
data CabalStep
  = DryRun
    -- ^ Run with @--dry-run@, only creating a plan.
  | OnlyDownload
    -- ^ Run with @--only-download@, creating a plan and only downloading
    -- dependencies.
  | OnlyDependencies
    -- ^ Run with @--only-dependencies@, creating a plan, downloading and
    -- building only the dependencies.
  | FullBuild
    -- ^ Run without any of the aforementioned options, fully building the
    -- selected targets.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A collection of values per each 'CabalStep'. Useful for "memoizing" a
-- function @'CabalStep' -> a@.
data PerCabalStep a = PerCabalStep
  { dryRun :: a
  , onlyDownload :: a
  , onlyDependencies :: a
  , fullBuild :: a
  } deriving stock (Functor, Foldable, Traversable)

indexCabalStep :: PerCabalStep a -> CabalStep -> a
indexCabalStep pcs = \case
  DryRun -> pcs.dryRun
  OnlyDownload -> pcs.onlyDownload
  OnlyDependencies -> pcs.onlyDependencies
  FullBuild -> pcs.fullBuild

-- | Note: the function is evaluated (to WHNF) at each input.
tabulateCabalStep' :: (CabalStep -> a) -> PerCabalStep a
tabulateCabalStep' f = PerCabalStep{..}
  where
    !dryRun = f DryRun
    !onlyDownload = f OnlyDownload
    !onlyDependencies = f OnlyDependencies
    !fullBuild = f FullBuild

modifyCabalStep :: CabalStep -> (a -> a) -> PerCabalStep a -> PerCabalStep a
modifyCabalStep step f pcs = case step of
  DryRun | !dryRun <- f pcs.dryRun
    -> pcs { dryRun }
  OnlyDownload | !onlyDownload <- f pcs.onlyDownload
    -> pcs { onlyDownload }
  OnlyDependencies | !onlyDependencies <- f pcs.onlyDependencies
    -> pcs { onlyDependencies }
  FullBuild | !fullBuild <- f pcs.fullBuild
    -> pcs { fullBuild }

setCabalStep :: CabalStep -> a -> PerCabalStep a -> PerCabalStep a
setCabalStep step value pcs = case step of
  DryRun -> pcs { dryRun = value }
  OnlyDownload -> pcs { onlyDownload = value }
  OnlyDependencies -> pcs { onlyDependencies = value }
  FullBuild -> pcs { fullBuild = value }

-- | In either case if we are in a cabal project then we can only install
-- packages that are in the dependency closure of the project, and they will be
-- subject to constraints defined in the project file, if any.
data CabalMode
  = ProjectBuild -- | Assume that we're in a cabal project and run @cabal build@
    -- targeting the project's packages and their dependencies.
  | InstallLib -- | Use @cabal install --lib@, which doesn't require to be in a
    -- cabal project. If we are in a project, the project's packages will be
    -- sdisted first.

-- | Options defining what cell in the build matrix we're in.
data Flavor = Flavor
  { unorderedOptions :: Set Text
  , orderedOptions :: [Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)

instance Semigroup Flavor where
  f1 <> f2 = Flavor
    { unorderedOptions = Set.union f1.unorderedOptions f2.unorderedOptions
    , orderedOptions = f1.orderedOptions ++ f2.orderedOptions
    }

instance Monoid Flavor where
  mempty = Flavor
    { unorderedOptions = Set.empty
    , orderedOptions = []
    }

-- | A single invocation of @cabal@.
data CabalArgs = CabalArgs
  { cabalExecutable :: FilePath
  , mode :: CabalMode
  , step :: CabalStep
  , options :: [Text]
  , targets :: [Text]
  , flavor :: Flavor
  }

data CabalRawArgs = CabalRawArgs
  { cabalExecutable :: FilePath
  , buildDir :: FilePath
    -- ^ @--builddir@, where build artifacts will be placed. Different
    -- flavors using the same compiler must use different 'buildDir'.
  , mode :: CabalMode
  , envFile :: Maybe FilePath
    -- ^ An environment file to use with @install --lib@. The file needs to not
    -- yet exist, so that its contents don't conflict with the install plan, but
    -- running cabal will bring this file into existence. So we basically have
    -- to remove this file every time.
  , step :: CabalStep
  , options :: [Text]
  , targets :: [Text]
  }

renderRawCabalArgs :: CabalRawArgs -> NonEmpty Text
renderRawCabalArgs ca = "cabal" :| mconcat
  [ case ca.mode of
    ProjectBuild -> ["build"]
    InstallLib -> ["install", "--lib"]
  , case ca.envFile of
    Nothing -> []
    Just path -> ["--package-env", Text.pack path]
  , ["--builddir", Text.pack ca.buildDir]
  , case ca.step of
    DryRun -> ["--dry-run"]
    OnlyDownload -> ["--only-download"]
    OnlyDependencies -> ["--only-dependencies"]
    FullBuild -> []
  , ca.options
  , ["--" | not $ null ca.targets]
  , ca.targets
  ]

argsToRaw :: CabalArgs -> CabalRawArgs
argsToRaw args@CabalArgs{..} = CabalRawArgs
  { cabalExecutable
  , buildDir = buildDirFor flavor
  , mode
  , envFile = environmentFilePath args
  , step
  , options = concat
    [ options
    , Set.toList flavor.unorderedOptions
    , flavor.orderedOptions
    ]
  , targets
  }

-- TODO: there probably needs to be some mechanism to clean up these at some
-- point.
buildDirFor :: Flavor -> FilePath
buildDirFor f = "dist-newstyle" </>
  ("cabal-matrix-" <> show (fromIntegral @Int @Word $ hash f))

renderCabalArgs :: CabalArgs -> NonEmpty Text
renderCabalArgs = renderRawCabalArgs . argsToRaw

environmentFilePath :: CabalArgs -> Maybe FilePath
environmentFilePath CabalArgs{..} = case mode of
  ProjectBuild -> Nothing
  InstallLib -> Just $ buildDirFor flavor </> "env"
