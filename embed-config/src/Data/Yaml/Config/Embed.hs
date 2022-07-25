-- | Reasonable conventions for embedding YAML configuration with Template Haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Yaml.Config.Embed
  ( module Data.Yaml.Config.Embed
  , AesonKebab(..)
  )
  where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, Value)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Yaml.Config.Kebab (AesonKebab(..))
import Language.Haskell.TH.Syntax (Exp, Q)

import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Config as Yaml.Config

-- | TH function for loading @config/settings.yml@ relative to
-- the project in which the TH splice is written.
--
-- > loadConfig :: IO MyConfig
-- > loadConfig = $(embedConfig)
embedConfig :: Q Exp
embedConfig = embedConfigRelativeToProject "config/settings.yml"

-- | TH function for loading the supplied config file path
-- relative to the project in which the TH splice is written.
--
-- > loadConfig :: IO MyConfig
-- > loadConfig = $(embedConfigRelativeToProject "path/to/settings.yml")
embedConfigRelativeToProject :: FilePath -> Q Exp
embedConfigRelativeToProject relPath = do
  absPath <- makeRelativeToProject relPath
  [| loadFromBytes $(embedFile absPath) |]

-- | Given the file content, read a YAML config file.
loadFromBytes :: (FromJSON a) => ByteString -> IO a
loadFromBytes bytes = do
  yaml <- either throwIO pure $ Yaml.decodeEither' bytes
  loadFromValue yaml

-- | Given the file content as an Aeson 'Value', read a YAML config file.
loadFromValue :: (FromJSON a) => Value -> IO a
loadFromValue yaml = do
  Yaml.Config.loadYamlSettings [] [yaml] Yaml.Config.useEnv
