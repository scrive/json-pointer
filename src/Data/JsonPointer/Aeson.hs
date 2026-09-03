{-# OPTIONS_GHC -Wno-orphans #-}

module Data.JsonPointer.Aeson where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as KM
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (toJSONKeyText, withText)
import Data.Maybe
import Data.Semigroup
import Data.Text (pack, unpack)
import Data.Vector qualified as Vector

import Data.JsonPointer.Model
import Data.JsonPointer.Parser

-- | Extract a pointed sub-value.
pointTo :: JsonPointer -> Aeson.Value -> Maybe Aeson.Value
pointTo pointer json = appEndo (getDual (runPointer pointer interpreter)) $ Just json
  where
    -- 'Dual' is what makes the reference tokens apply left to right:
    -- the 'Semigroup' of 'Endo' is function composition.
    interpreter index key = Dual $ Endo (lookup' =<<)
      where
        lookup' = \case
          Aeson.Object x -> KM.lookup (KM.fromText key) x
          Aeson.Array x -> (Vector.!?) x =<< index
          _ -> Nothing

-- | Like 'pointTo', but returns 'Aeson.Null' if the pointer does not resolve.
pointToNullable :: JsonPointer -> Aeson.Value -> Aeson.Value
pointToNullable pointer json = fromMaybe Aeson.Null $ pointTo pointer json

-- | Parse both the plain and the relative URI form.
--
-- See `parseJsonPointer` for the details.
instance FromJSON JsonPointer where
  parseJSON = withText "JsonPointer" $ either (fail . unpack) pure . parseJsonPointer

-- | Parse both the plain and the relative URI form.
--
-- See `parseJsonPointer` for the details.
instance FromJSONKey JsonPointer where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . unpack) pure . parseJsonPointer

-- | Render the plain form, e.g., @\/foo\/bar@
instance ToJSON JsonPointer where
  toJSON p = Aeson.toJSON $ show p

-- | Render the plain form, e.g., @\/foo\/bar@
instance ToJSONKey JsonPointer where
  toJSONKey = toJSONKeyText $ pack . show
