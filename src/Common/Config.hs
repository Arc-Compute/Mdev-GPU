{-|
Module      : Common.Config
Description : YAML + JSON Parsing of configuration files.
Copyright   : (c) 2022 2666680 Ontario Inc. O\A Arc Compute
License     : GNU GPL v.2
Maintainer  : michael@arccompute.io
Stability   : experimental
Portability : POSIX

Configuration module to handle YAML + JSON parsing.
-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Config where

import Data.Aeson (FromJSON(..), decodeFileStrict)
import Data.Text (Text, isSuffixOf, unpack)
import Path.Posix (parseSomeFile, SomeBase(..), Path(..))
import YamlParse.Applicative

import Common.Types

{-| Mdev configuration layer to help act as a high level wrapper.
-}
data MdevCliConfig = MdevCliConfig
  { gpus :: [GpuConfig]                 -- ^ List of all gpu configurations.
  } deriving (Show)

instance YamlSchema MdevCliConfig where
  yamlSchema =
    objectParser "MdevCliConfig" $
      MdevCliConfig
        <$> requiredField "gpus" "GPU configuration choices to use."

instance FromJSON MdevCliConfig where
  parseJSON = viaYamlSchema

{-| Wrapper around possible GPU objects.
-}
data GpuConfig = GpuConfig
  { filteredGpus :: [RequestedGpu]    -- ^ Implement the following GPUs on the filtered GPU contexts.
  , mdevConfigs :: [MDevRequest]      -- ^ List of mdevs to create.
  } deriving (Show)

instance YamlSchema GpuConfig where
  yamlSchema =
    objectParser "GpuConfig" $
      GpuConfig
        <$> optionalFieldWithDefault "filteredGpus" [] "Adds the mdev requests to the following filter list."
        <*> requiredField "mdevConfigs" "The MDev requests to create the VMs."

instance FromJSON GpuConfig where
  parseJSON = viaYamlSchema

{-| Gets the configuration script and parses it from the system.
-}
getConfig :: Text                    -- ^ File path to parse.
  -> IO ([GpuConfig])                -- ^ Returns the GPU configuration list.
getConfig n =
  if isSuffixOf ".json" n
    then do
      Just mdev <- decodeFileStrict (unpack n) :: IO (Maybe MdevCliConfig)
      return $ gpus mdev
    else do
      d <- parseSomeFile (unpack n)
      case d of
        Rel f -> do
          Just mdev <- readConfigFile f :: IO (Maybe MdevCliConfig)
          return $ gpus mdev
        Abs f -> do
          Just mdev <- readConfigFile f :: IO (Maybe MdevCliConfig)
          return $ gpus mdev
