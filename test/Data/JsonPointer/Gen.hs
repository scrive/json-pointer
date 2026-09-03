-- |
-- Generators for property tests.
module Data.JsonPointer.Gen
  ( Key (..)
  , Pointer (..)
  )
where

import Data.JsonPointer.Model
import Data.Text qualified as T
import Test.QuickCheck

-- |
-- A reference token, biased towards the characters
-- that need escaping in the JSON Pointer syntax.
newtype Key = Key T.Text
  deriving stock (Eq, Show)

instance Arbitrary Key where
  arbitrary = Key . T.pack <$> listOf (elements "ab01~/ ")
  shrink (Key text) = Key . T.pack <$> shrink (T.unpack text)

-- |
-- A JSON Pointer assembled out of arbitrary 'Key's.
newtype Pointer = Pointer JsonPointer
  deriving newtype (Eq, Show)

instance Arbitrary Pointer where
  arbitrary = fromKeys <$> arbitrary
  shrink (Pointer pointer) =
    fromKeys <$> shrink (run pointer (\_ key -> [Key key]))

fromKeys :: [Key] -> Pointer
fromKeys = Pointer . foldMap (\(Key key) -> atKey key)
