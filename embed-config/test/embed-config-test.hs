{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson (FromJSON)
import Data.Yaml.Config.Embed (AesonKebab(..), embedConfig, embedConfigRelativeToProject)
import GHC.Generics (Generic)
import System.Environment (setEnv, unsetEnv)
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec do
  describe "embedConfig" do
    it "default-environment" do
      unsetEnv "FOO_BAR"
      unsetEnv "BAZ_QUUX"
      $(embedConfig) `shouldReturn`
        Config1
          { fooBar = 1
          , bazQuux = "two"
          }
    it "custom-environment" do
      setEnv "FOO_BAR" "7"
      setEnv "BAZ_QUUX" "twelve"
      $(embedConfig) `shouldReturn`
        Config1
          { fooBar = 7
          , bazQuux = "twelve"
          }
  describe "embedConfigRelativeToProject" do
    it "default-environment" do
      unsetEnv "SPAM_EGGS"
      unsetEnv "PARROT_ZOOM"
      $(embedConfigRelativeToProject "config/settings2.yml") `shouldReturn`
        Config2
          { spamEggs = 3
          , parrotZoom = "hi"
          }
    it "custom-environment" do
      setEnv "SPAM_EGGS" "255"
      setEnv "PARROT_ZOOM" "bye"
      $(embedConfigRelativeToProject "config/settings2.yml") `shouldReturn`
        Config2
          { spamEggs = 255
          , parrotZoom = "bye"
          }

data Config1 = Config1
  { fooBar :: Int
  , bazQuux :: String
  } deriving stock (Show, Eq, Generic)
    deriving (FromJSON) via AesonKebab Config1

data Config2 = Config2
  { spamEggs :: Int
  , parrotZoom :: String
  } deriving stock (Show, Eq, Generic)
    deriving (FromJSON) via AesonKebab Config2
