{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- OpenAPI schema of the JSON representation
-- provided by "Data.JsonPointer.Aeson".
module Data.JsonPointer.OpenApi () where

import Data.Aeson qualified as Aeson
import Data.Function
import Data.OpenApi
import Data.OpenApi.Optics ()
import Optics.Core

import Data.JsonPointer.Aeson ()
import Data.JsonPointer.Model

instance ToSchema JsonPointer where
  declareNamedSchema _ =
    pure . NamedSchema (Just "JsonPointer") $
      mempty
        & #type ?~ OpenApiString
        & #format ?~ "json-pointer"
        & #description
          ?~ "A JSON Pointer as defined by RFC 6901. \
             \Both the plain form '/foo/bar' and the #-prefixed form '#/foo/bar' are accepted, \
             \but neither of them is URL-encoded."
        & #example ?~ Aeson.toJSON (atKey "foo" <> atIndex 0 <> atKey "bar")
