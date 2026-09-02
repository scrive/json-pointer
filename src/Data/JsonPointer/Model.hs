module Data.JsonPointer.Model
  ( JsonPointer
  , run
  , atIndexOrKey
  , atIndex
  , atKey
  , escapeKey
  , unescapeKey
  )
where

import Data.Text qualified as T
import Data.Semigroup

-- |
-- A model of JsonPointer
-- represented in terms of a monoid.
newtype JsonPointer
  = JsonPointer (forall m. Monoid m => (Maybe Int -> T.Text -> m) -> m)

instance Semigroup JsonPointer where
  {-# INLINE (<>) #-}
  JsonPointer fn1 <> JsonPointer fn2 =
    JsonPointer $ \handler -> fn1 handler <> fn2 handler

instance Monoid JsonPointer where
  {-# INLINE mempty #-}
  mempty = JsonPointer $ const mempty

instance Show JsonPointer where
  showsPrec _ (JsonPointer impl) =
    appEndo $ impl (\_ text -> Endo (showString "/" . showString (T.unpack $ escapeKey text)))

instance Eq JsonPointer where
  a == b = show a == show b

instance Ord JsonPointer where
  a <= b = show a <= show b

-- |
-- Given a JSON Pointer specification and a function,
-- which interprets a possible index or a textual key into a monoid,
-- results in such a monoid.
{-# INLINE run #-}
run :: Monoid m => JsonPointer -> (Maybe Int -> T.Text -> m) -> m
run (JsonPointer fn) = fn

-- |
-- Constructs JSON Pointer from a possible array index and a textual key.
{-# INLINE atIndexOrKey #-}
atIndexOrKey :: Maybe Int -> T.Text -> JsonPointer
atIndexOrKey index key = JsonPointer $ \handler -> handler index key

-- |
-- Constructs JSON Pointer from an index
atIndex :: Int -> JsonPointer
atIndex index = JsonPointer $ \handler -> handler (Just index) (T.pack $ show index)

-- |
-- Constructs JSON Pointer from a textual key.
{-# INLINE atKey #-}
atKey :: T.Text -> JsonPointer
atKey = atIndexOrKey Nothing

-- |
-- Escape JSON Pointer string.
-- See here https://datatracker.ietf.org/doc/html/rfc6901 for more details.
escapeKey :: T.Text -> T.Text
escapeKey = T.replace "/" "~1" . T.replace "~" "~0"

-- |
-- Unscape JSON Pointer string.
-- See here https://datatracker.ietf.org/doc/html/rfc6901 for more details.
unescapeKey :: T.Text -> T.Text
unescapeKey = T.replace "~0" "~" . T.replace "~1" "/"
