-- |
-- Re-exports of the whole library.
module Data.JsonPointer
  ( module Data.JsonPointer.Model
  , module Data.JsonPointer.Parser
  , module Data.JsonPointer.Aeson
  )
where

import Data.JsonPointer.Aeson
import Data.JsonPointer.Model

-- Data.JsonPointer.OpenApi holds instances only, hence it is not in the list above.
import Data.JsonPointer.OpenApi ()
import Data.JsonPointer.Parser
