module JSONPointer.Model
  ( JSONPointer
  , run
  , atIndexOrKey
  , atKey
  , escapeKey
  , unescapeKey
  )
where

import Data.Text qualified as T
import Data.Semigroup

-- |
-- A model of JSONPointer
-- represented in terms of a monoid.
newtype JSONPointer
  = JSONPointer (forall m. Monoid m => (Maybe Int -> T.Text -> m) -> m)

instance Semigroup JSONPointer where
  {-# INLINE (<>) #-}
  JSONPointer fn1 <> JSONPointer fn2 =
    JSONPointer $ \handler -> fn1 handler <> fn2 handler

instance Monoid JSONPointer where
  {-# INLINE mempty #-}
  mempty = JSONPointer $ const mempty

instance Show JSONPointer where
  showsPrec _ (JSONPointer impl) =
    appEndo $ impl (\_ text -> Endo (showString "/" . showString (T.unpack $ escapeKey text)))

instance Eq JSONPointer where
  a == b = show a == show b

instance Ord JSONPointer where
  a <= b = show a > show b

-- |
-- Given a JSON Pointer specification and a function,
-- which interprets a possible index or a textual key into a monoid,
-- results in such a monoid.
{-# INLINE run #-}
run :: Monoid m => JSONPointer -> (Maybe Int -> T.Text -> m) -> m
run (JSONPointer fn) = fn

-- |
-- Constructs JSON Pointer from a possible array index and a textual key.
{-# INLINE atIndexOrKey #-}
atIndexOrKey :: Maybe Int -> T.Text -> JSONPointer
atIndexOrKey index key = JSONPointer $ \handler -> handler index key

-- |
-- Constructs JSON Pointer from a textual key.
{-# INLINE atKey #-}
atKey :: T.Text -> JSONPointer
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
