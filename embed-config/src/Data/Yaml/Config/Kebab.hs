-- | Provides kebab-case instances for @aeson@.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Yaml.Config.Kebab where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic, Rep)

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Language.Haskell.TH.Syntax as TH

-- | A DerivingVia wrapper that only turns all fields into kebab-case.
-- No other field processing occurs, which also means no field prefix stripping.
newtype AesonKebab a = AesonKebab a

-- | Default 'Aeson.Options' which sets 'Aeson.fieldLabelModifier' to use 'kebab'.
aesonKebabOptions :: Aeson.Options
aesonKebabOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = kebab
    }

-- | Convert a @camelCase@ string to @kebab-case@.
kebab :: String -> String
kebab s = do
  c <- s
  if Char.isUpper c then ['-', Char.toLower c] else [c]

-- | Gets the given identifier name as a 'String' but converts it
-- to @kebab-case@. Useful for
--
-- > fooBar = True
-- > kebabName 'fooBar == "foo-bar"
kebabName :: TH.Name -> String
kebabName = kebab . getName

-- | Gets the given identifier name as a 'String'.
--
-- If @DuplicateRecordFields@ is enabled, detects the names in the form of
-- @$sel:name:Type@ and extracts the @name@.
getName :: TH.Name -> String
getName (TH.Name (TH.OccName s) _) =
  case s of
    '$' : _ -> takeWhile (/= ':') (drop 5 s)
    _ -> s

instance
  ( Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (AesonKebab a)
  where
  toJSON (AesonKebab a) = Aeson.genericToJSON aesonKebabOptions a
  toEncoding (AesonKebab a) = Aeson.genericToEncoding aesonKebabOptions a

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (AesonKebab a)
  where
  parseJSON v = AesonKebab <$> Aeson.genericParseJSON aesonKebabOptions v
