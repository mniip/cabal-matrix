module Cabal.Matrix.RecordResult
  ( RecordResult(..)
  , FlavorResult(..)
  , StepResult(..)
  ) where

import Cabal.Matrix.ProcessRunner
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)


-- | The ultimate result of having completed a single step of a single flavor.
data StepResult = StepResult
  { cmdline :: NonEmpty Text
  , output :: [(OutputChannel, Text)]
  , exitCode :: Int
  }

instance ToJSON StepResult where
  toJSON sr = Aeson.object
    [ "cmdline" Aeson..= sr.cmdline
    , "output" Aeson..= sr.output
    , "exit_code" Aeson..= sr.exitCode
    ]
  -- Have to manually define toEncoding to force field ordering.
  toEncoding sr = Encoding.pairs $ mconcat
    [ Encoding.pair "cmdline" $ Aeson.toEncoding sr.cmdline
    , Encoding.pair "output" $ Aeson.toEncoding sr.output
    , Encoding.pair "exit_code" $ Aeson.toEncoding sr.exitCode
    ]

instance FromJSON StepResult where
  parseJSON = Aeson.withObject "StepResult" \o -> do
    cmdline <- o Aeson..: "cmdline"
    output <- o Aeson..: "output"
    exitCode <- o Aeson..: "exit_code"
    pure StepResult{..}

-- | The ultimate result of having processed a single flavor in a build matrix.
data FlavorResult = FlavorResult
  { flavor :: Map Text Text
  , dryRun :: Maybe StepResult
  , onlyDownload :: Maybe StepResult
  , onlyDependencies :: Maybe StepResult
  , fullBuild :: Maybe StepResult
  }

instance ToJSON FlavorResult where
  toJSON fr = Aeson.object $ catMaybes
    [ ("flavor" Aeson..=) <$> Just fr.flavor
    , ("dry_run" Aeson..=) <$> fr.dryRun
    , ("only_download" Aeson..=) <$> fr.onlyDownload
    , ("only_dependencies" Aeson..=) <$> fr.onlyDependencies
    , ("full_build" Aeson..=) <$> fr.fullBuild
    ]
  -- Have to manually define toEncoding to force field ordering.
  toEncoding fr = Encoding.pairs $ fold
    [ Encoding.pair "flavor" $ Aeson.toEncoding fr.flavor
    , foldMap (Encoding.pair "dry_run" . Aeson.toEncoding) fr.dryRun
    , foldMap (Encoding.pair "only_download" . Aeson.toEncoding) fr.onlyDownload
    , foldMap (Encoding.pair "only_dependencies" . Aeson.toEncoding)
      fr.onlyDependencies
    , foldMap (Encoding.pair "full_build" . Aeson.toEncoding) fr.fullBuild
    ]

instance FromJSON FlavorResult where
  parseJSON = Aeson.withObject "FlavorResult" \o -> do
    flavor <- o Aeson..: "flavor"
    dryRun <- o Aeson..:? "dry_run"
    onlyDownload <- o Aeson..:? "only_download"
    onlyDependencies <- o Aeson..:? "only_dependencies"
    fullBuild <- o Aeson..:? "full_build"
    pure FlavorResult{..}

-- | The ultimate result of having run a build matrix.
newtype RecordResult = RecordResult [FlavorResult]
  deriving newtype (FromJSON, ToJSON)
