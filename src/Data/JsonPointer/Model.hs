module Data.JsonPointer.Model
  ( JsonPointer
  , run
  , atIndex
  , atKey
  , escapeKey
  , unescapeKey
  )
where

import Data.Bits
import Data.Semigroup
import Data.Text qualified as T
import Data.Text.Read qualified as TR

-- | JsonPointer represented in terms of a monoid.
--
-- For numerical indexes, the index is stored in both arguments.
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

-- The rendered pointer determines the reference tokens,
-- which in turn determine the indices, hence comparing the renderings
-- is the same as comparing the pointers.
instance Eq JsonPointer where
  a == b = show a == show b

instance Ord JsonPointer where
  a <= b = show a <= show b

-- | Given a JsonPointer and a function,
-- which interprets a possible index or a textual key into a monoid,
-- return the result of applying the function to the pointer.
{-# INLINE run #-}
run :: Monoid m => JsonPointer -> (Maybe Int -> T.Text -> m) -> m
run (JsonPointer fn) = fn

-- | The array index a reference token denotes, if any.
--
-- An index must not begin with a zero, as per RFC 6901,
-- and one that does not fit into an 'Int' cannot address an array either,
-- so both of those are plain keys.
tokenIndex :: T.Text -> Maybe Int
tokenIndex token
  | T.length token > 1, T.head token == '0' = Nothing
  | otherwise = case TR.decimal @Integer token of
      Right (index, rest) | T.null rest -> toIntegralSized index
      _ -> Nothing

-- | Construct JSON Pointer from an index
atIndex :: Int -> JsonPointer
atIndex = atKey . T.pack . show

-- | Construct a JsonPointer from a single reference token.
--
-- If the key is a number, it can index into an array, as well as an object.
{-# INLINE atKey #-}
atKey :: T.Text -> JsonPointer
atKey key = JsonPointer $ \handler -> handler (tokenIndex key) key

-- | Escape JSON Pointer string
--
-- See here https://datatracker.ietf.org/doc/html/rfc6901 for more details.
escapeKey :: T.Text -> T.Text
escapeKey = T.replace "/" "~1" . T.replace "~" "~0"

-- | Unscape JSON Pointer string
--
-- See here https://datatracker.ietf.org/doc/html/rfc6901 for more details.
unescapeKey :: T.Text -> T.Text
unescapeKey = T.replace "~0" "~" . T.replace "~1" "/"
