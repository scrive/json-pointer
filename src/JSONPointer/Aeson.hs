{-# OPTIONS_GHC -Wno-orphans #-}

module JSONPointer.Aeson where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (withText)
import Data.Text (unpack)
import Data.Semigroup
import Data.Maybe
import Data.Vector qualified as Vector

import JSONPointer.Model
import JSONPointer.Parser

-- |
-- Converts JSONPointer into an Aeson Value lookup function.
value :: JSONPointer -> Aeson.Value -> Maybe Aeson.Value
value pointer json = appEndo (run pointer interpreter) $ Just json
  where
    interpreter index key = Endo (lookup' =<<)
      where
        lookup' = \case
          Aeson.Object x -> KM.lookup (KM.fromText key) x
          Aeson.Array x -> (Vector.!?) x =<< index
          _ -> Nothing

nullableValue :: JSONPointer -> Aeson.Value -> Aeson.Value
nullableValue pointer json = fromMaybe Aeson.Null $ value pointer json

instance FromJSON JSONPointer where
  parseJSON = withText "JSONPointer" $ \t ->
    case parse jsonPointerUriFragment t of
      Left err -> fail $ unpack err
      Right x -> pure x
