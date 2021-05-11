module Cloud.PubSub.Core.Json
  ( lowercaseFirstChar
  , options
  ) where

import qualified Data.Aeson                    as Aeson
import qualified Data.Char                     as Char

lowercaseFirstChar :: String -> String
lowercaseFirstChar (x : xs) = Char.toLower x : xs
lowercaseFirstChar []       = []

options :: Aeson.Options
options = Aeson.defaultOptions
  { Aeson.constructorTagModifier = map Char.toUpper . Aeson.camelTo2 '_'
  , Aeson.omitNothingFields      = True
  }
