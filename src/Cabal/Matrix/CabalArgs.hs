-- | Converting high-level build matrix concepts into @cabal@ commandlines.
-- Some instructions cannot be passed via the command line, and instead have to
-- be passed through a temporary @cabal.project@ file.
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
  , Conjunction(..)
  , Disjunction(..)
  , Constraint(..)
  , renderCabalArgs
  , UserProjectFiles
  , detectUserProjectFiles
  , prepareFilesForCabal
  ) where

import Control.Exception.Safe
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Distribution.Package
import Distribution.Pretty
import Distribution.Version
import GHC.Generics
import System.FilePath
import System.Directory


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
  | BlankProjectBuild -- | Create a temporary cabal project containing the
    -- targets as @extra-packages@ (meaning they are considered local packages,
    -- not included in 'OnlyDependencies') and run @cabal build@ with that
    -- project. This is a much faster alternative to 'InstallLib', because cabal
    -- doesn't need to read the installed package store.
  | InstallLib -- | Use @cabal install --lib@, which doesn't require to be in a
    -- cabal project. If we are in a project, the project's packages will be
    -- sdisted first.
  deriving stock (Eq)

data Constraint = Constraint
  { package :: PackageName
  , versions :: VersionRange
  } deriving stock (Eq, Show, Generic)

-- Cabal-syntax 3.8 doesn't have @Ord Constraint@
instance Ord Constraint where
  compare c1 c2 = compare c1.package c2.package
    <> compare (prettyShow c1.versions) (prettyShow c2.versions)

instance Hashable Constraint where
  hashWithSalt salt constr = salt
    `hashWithSalt` unPackageName constr.package
    `hashWithSalt` prettyShow constr.versions

-- | All of these must be satisfied.
newtype Conjunction a = Conjunction { unConjunction :: Set a }
  deriving newtype (Eq, Ord, Hashable)
  deriving stock (Show)

-- | Any of these must be satisfied. To specify a disjunction of cabal
-- constraints we have to express them as cabal flags on a fake package.
newtype Disjunction a = Disjunction { unDisjunction :: Set a }
  deriving newtype (Eq, Ord, Hashable)
  deriving stock (Show)

-- | Options defining what cell in the build matrix we're in.
data Flavor = Flavor
  { constraints :: Conjunction (Disjunction (Conjunction Constraint))
  , unorderedOptions :: Set Text
  , orderedOptions :: [Text]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Hashable)

instance Semigroup Flavor where
  f1 <> f2 = Flavor
    { constraints = Conjunction
      $ Set.union f1.constraints.unConjunction f2.constraints.unConjunction
    , unorderedOptions = Set.union f1.unorderedOptions f2.unorderedOptions
    , orderedOptions = f1.orderedOptions ++ f2.orderedOptions
    }

instance Monoid Flavor where
  mempty = Flavor
    { constraints = Conjunction Set.empty
    , unorderedOptions = Set.empty
    , orderedOptions = []
    }

-- | A single invocation of @cabal@.
data CabalArgs = CabalArgs
  { cabalExecutable :: FilePath
  , userProjectFiles :: UserProjectFiles
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
  , projectFile :: Maybe FilePath
    -- ^ A project file to use with @cabal build@.
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
    BlankProjectBuild -> ["build"]
    InstallLib -> ["install", "--lib"]
  , case ca.projectFile of
    Nothing -> []
    Just path -> ["--project-file", Text.pack path]
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
  , projectFile = fst . (.projectFile) <$> mProjectFile
  , step
  , options = concat
    [ options
    , Set.toList flavor.unorderedOptions
    , projectArgs
    , flavor.orderedOptions
    ]
  , targets
  }
  where
    (projectArgs, mProjectFile) = projectOrArgs args

-- TODO: there probably needs to be some mechanism to clean up these at some
-- point.
buildDirFor :: Flavor -> FilePath
buildDirFor f = "dist-newstyle" </>
  ("cabal-matrix-" <> show (fromIntegral @Int @Word $ hash f))

renderCabalArgs :: CabalArgs -> NonEmpty Text
renderCabalArgs = renderRawCabalArgs . argsToRaw

data ProjectData = ProjectData
  { projectFile :: (FilePath, Text)
  , auxCabalFile :: Maybe (FilePath, Text)
  }

-- | Do we need a project file, or will some CLI args suffice?
projectOrArgs :: CabalArgs -> ([Text], Maybe ProjectData)
projectOrArgs args =
  ( cliConstraints
  , guard (args.mode == BlankProjectBuild || needAuxCabalFile)
    >> Just ProjectData
      { projectFile = (projectFile, projectFileContents)
      , auxCabalFile = guard needAuxCabalFile >> Just
        ( auxCabalFile
        , auxCabalFileContents
        )
      }
  )
  where
    (Set.unions -> simpleConstraints, complexConstraints) = partitionEithers
      $ Set.toList args.flavor.constraints.unConjunction <&> \disj
        -> case NonEmpty.nonEmpty $ Set.toList disj.unDisjunction of
          Nothing -> Left $ Set.singleton Constraint
            { package = "base"
            , versions = noVersion
            } -- An empty disjunction is an unsatisfiable constraint.
          Just (conj :| []) -> Left conj.unConjunction
          Just conjs -> Right conjs

    cliConstraints =
      [ "--constraint=" <> Text.pack (unPackageName package)
        <> Text.pack (prettyShow versions)
      | Constraint{ package, versions } <- Set.toList simpleConstraints
      ]

    needAuxCabalFile = not $ null complexConstraints

    projectFile = buildDirFor args.flavor </> "cabal.project"
    projectFileContents = Text.unlines $
      [ "import: " <> Text.pack userProjectFile
      | args.mode /= BlankProjectBuild
      , userProjectFile <- args.userProjectFiles.unUserProjectFiles
      ] <>
      [ "extra-packages: " <> Text.intercalate ", " args.targets
      | args.mode == BlankProjectBuild
      ] <>
      [ "packages: " <> Text.pack auxCabalFile
      | needAuxCabalFile
      ]

    auxCabalFile = buildDirFor args.flavor
      </> "cabal-matrix-constraints-fake-package.cabal"
    (flags, buildDepends) = complexConstraintsToFlags complexConstraints
    auxCabalFileContents = Text.unlines $
      [ "cabal-version: >=1.8"
      , "name: cabal-matrix-constraints-fake-package"
      , "version: 0"
      , "build-type: Simple"
      ] <>
      [ "flag " <> flag <> " {}"
      | flag <- flags
      ] <>
      [ "library"
      ] <>
      buildDepends

complexConstraintsToFlags
  :: [NonEmpty (Conjunction Constraint)] -> ([Text], [Text])
complexConstraintsToFlags complexConstraints =
  ( [ flag i j
    | (i, conjs) <- zip [0..] complexConstraints
    , j <- [1 .. NonEmpty.length conjs - 1]
    ]
  , concat $ zipWith (goDisj 1 1) [0..] complexConstraints
  )
  where
    goDisj !indent !_j !_i (conj :| []) = goConj indent conj
    goDisj indent j i (conj :| conj' : conjs) =
      [ Text.replicate indent "  " <> "if flag(" <> flag i j <> ") {"
      ] <> goConj (indent + 1) conj <>
      [ Text.replicate indent "  " <> "} else {"
      ] <> goDisj (indent + 1) (j + 1) i (conj' :| conjs) <>
      [ Text.replicate indent "  " <> "}"
      ]

    goConj !indent (Conjunction constrs) =
      [ Text.replicate indent "  " <> "build-depends:"
        <> Text.intercalate ", "
        [ Text.pack $ prettyShow constr.package <> prettyShow constr.versions
        | constr <- Set.toList constrs
        ]
      ]

    flag :: Int -> Int -> Text
    flag i j = "cabal-matrix-disjunction-"
      <> Text.pack (show i) <> "-" <> Text.pack (show j)

environmentFilePath :: CabalArgs -> Maybe FilePath
environmentFilePath CabalArgs{..} = case mode of
  InstallLib -> Just $ buildDirFor flavor </> "env"
  ProjectBuild -> Nothing
  BlankProjectBuild -> Nothing

newtype UserProjectFiles = UserProjectFiles { unUserProjectFiles :: [FilePath] }

detectUserProjectFiles :: IO UserProjectFiles
detectUserProjectFiles = UserProjectFiles <$> (getCurrentDirectory >>= go)
  where
    go dir = doesFileExist projectFile >>= \case
      False -> if takeDirectory dir /= dir
        then go $ takeDirectory dir
        else pure []
      True -> (projectFile:) <$> filterM doesFileExist [localFile, freezeFile]
      where
        projectFile = dir </> "cabal.project"
        localFile = dir </> "cabal.project.local"
        freezeFile = dir </> "cabal.project.freeze"

prepareFilesForCabal :: CabalArgs -> IO ()
prepareFilesForCabal args = do
  for_ (snd $ projectOrArgs args) \project -> do
    createDirectoryIfMissing True $ takeDirectory $ fst project.projectFile
    uncurry Text.writeFile project.projectFile
    for_ project.auxCabalFile \auxCabalFile -> do
      createDirectoryIfMissing True $ takeDirectory $ fst auxCabalFile
      uncurry Text.writeFile auxCabalFile
  for_ (environmentFilePath args) \environmentFile -> do
    void $ try @_ @IOError $ removeFile environmentFile
