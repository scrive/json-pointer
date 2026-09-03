{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI schema of the JSON representation provided by "Data.JsonPointer.Aeson".
module Data.JsonPointer.OpenApi () where

import Data.Aeson qualified as Aeson
import Data.OpenApi

import Data.JsonPointer.Aeson ()
import Data.JsonPointer.Model

instance ToSchema JsonPointer where
  declareNamedSchema _ =
    pure . NamedSchema (Just "JsonPointer") $
      mempty
        { _schemaType = Just OpenApiString
        , _schemaFormat = Just "json-pointer"
        , _schemaDescription =
            Just
              "A JSON Pointer as defined by RFC 6901. \
              \Both the plain form '/foo/bar' and the #-prefixed form '#/foo/bar' are accepted, \
              \but neither of them is URL-encoded."
        , _schemaExample = Just $ Aeson.toJSON (atKey "foo" <> atIndex 0 <> atKey "bar")
        }
