{-# OPTIONS_GHC -Wno-orphans #-}

module Data.JsonPointer.Aeson where

-- import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
-- import Data.Aeson.Types (withText)
-- import Data.Text (unpack)
import Data.Semigroup
import Data.Maybe
import Data.Vector qualified as Vector
-- import Network.HTTP.Types.URI

import Data.JsonPointer.Model
-- import Data.JsonPointer.Parser

-- |
-- Converts JsonPointer into an Aeson Value lookup function.
value :: JsonPointer -> Aeson.Value -> Maybe Aeson.Value
value pointer json = appEndo (getDual (run pointer interpreter)) $ Just json
  where
    -- 'Dual' is what makes the reference tokens apply left to right:
    -- the 'Semigroup' of 'Endo' is function composition.
    interpreter index key = Dual $ Endo (lookup' =<<)
      where
        lookup' = \case
          Aeson.Object x -> KM.lookup (KM.fromText key) x
          Aeson.Array x -> (Vector.!?) x =<< index
          _ -> Nothing

nullableValue :: JsonPointer -> Aeson.Value -> Aeson.Value
nullableValue pointer json = fromMaybe Aeson.Null $ value pointer json

-- instance FromJSON JsonPointer where
--   parseJSON = withText "JsonPointer" $ \t ->
--     case parse jsonPointerUriFragment t of
--       Left err -> fail $ unpack err
--       Right x -> pure x

-- instance ToJSON JsonPointer where
--   toJSON p = Aeson.toJSON $ show p


-- instance FromJSON JsonPointerUriFragment where
--   parseJSON = withText "JsonPointer" $ \t ->
--     case parse jsonPointerUriFragment t of
--       Left err -> fail $ unpack err
--       Right x -> pure x

-- instance ToJSON JsonPointerUriFragment where
--   toJSON p = Aeson.toJSON $ show p


-- newtype JsonPointerUriFragment = JsonPointerUriFragment JsonPointer

-- instance Show JsonPointerUriFragment where
--   showsPrec _ (JsonPointerUriFragment p) = "#" <> show p

