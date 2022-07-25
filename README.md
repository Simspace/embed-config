# embed-config [![embed-config](https://img.shields.io/hackage/v/embed-config.svg?logo=haskell&color=blueviolet)](https://hackage.haskell.org/package/embed-config)

Reasonable conventions for embedding YAML configuration with Template Haskell

## Quick Start

**config/settings.yml**
```yaml
foo-bar: 42
baz-quux: hello
```

**src/MyApp/Config.hs**
```haskell
module MyApp.Config where

import Data.Yaml.Config.Embed (AesonKebab(..), embedConfig)

loadConfig :: IO Config
loadConfig = $(embedConfig)

data Config = Config
  { fooBar :: Int
  , bazQuux :: String
  } deriving stock (Show, Eq, Generic)
    deriving (FromJSON) via AesonKebab Config
```
