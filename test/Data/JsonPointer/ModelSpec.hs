module Data.JsonPointer.ModelSpec (spec) where

import Data.JsonPointer.Gen
import Data.JsonPointer.Model
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck

-- The monoid law properties below are the point of these tests.
{-# ANN module ("HLint: ignore Monoid law, left identity" :: String) #-}

{-# ANN module ("HLint: ignore Monoid law, right identity" :: String) #-}

spec :: Spec
spec = do
  describe "escapeKey" $ do
    it "escapes a slash as ~1" $
      escapeKey "a/b" `shouldBe` "a~1b"

    it "escapes a tilde as ~0" $
      escapeKey "a~b" `shouldBe` "a~0b"

    it "does not double-escape an already escaped sequence" $
      escapeKey "a~1b" `shouldBe` "a~01b"

    it "leaves a plain key untouched" $
      escapeKey "foo" `shouldBe` "foo"

  describe "unescapeKey" $
    prop "inverts escapeKey" $ \(Key key) ->
      unescapeKey (escapeKey key) `shouldBe` key

  describe "show" $ do
    it "renders the empty pointer as the empty string" $
      show (mempty @JsonPointer) `shouldBe` ""

    it "separates the reference tokens with slashes" $
      show (atKey "foo" <> atKey "bar") `shouldBe` "/foo/bar"

    it "renders an index as its decimal representation" $
      show (atKey "foo" <> atIndex 12) `shouldBe` "/foo/12"

    it "escapes the reference tokens" $
      show (atKey "a/b" <> atKey "a~b") `shouldBe` "/a~1b/a~0b"

    it "renders an empty reference token" $
      show (atKey "") `shouldBe` "/"

  describe "Monoid laws" $ do
    prop "left identity" $ \(Pointer pointer) ->
      mempty <> pointer `shouldBe` pointer

    prop "right identity" $ \(Pointer pointer) ->
      pointer <> mempty `shouldBe` pointer

    prop "associativity" $ \(Pointer a) (Pointer b) (Pointer c) ->
      (a <> b) <> c `shouldBe` a <> (b <> c)

  describe "runPointer" $ do
    it "hands over the keys in order" $
      runPointer (atKey "foo" <> atIndex 3) (\_ key -> [key]) `shouldBe` ["foo", "3"]

    it "hands over an index only for a numeric reference token" $
      runPointer (atKey "foo" <> atIndex 3) (\index _ -> [index]) `shouldBe` [Nothing, Just 3]

  describe "Ord" $
    prop "agrees with the ordering of the rendered pointers" $ \(Pointer a) (Pointer b) ->
      compare a b `shouldBe` compare (show a) (show b)

  describe "atKey" $ do
    prop "is the escaped key prefixed with a slash" $ \(Key key) ->
      show (atKey key) `shouldBe` T.unpack ("/" <> escapeKey key)

    it "derives the array index from the key" $
      indicesOf (atKey "12") `shouldBe` [Just 12]

    it "derives no index for a key with a leading zero" $
      indicesOf (atKey "012") `shouldBe` [Nothing]

    it "derives no index for a non-numeric key" $
      indicesOf (atKey "foo") `shouldBe` [Nothing]

    it "derives no index for a number that does not fit into an Int" $
      indicesOf (atKey "18446744073709551617") `shouldBe` [Nothing]

    it "keeps the key as it is given" $
      runPointer (atKey "a/b") (\_ key -> [key]) `shouldBe` ["a/b"]

  describe "atIndex" $ do
    it "agrees with the key it renders as" $
      atIndex 12 `shouldBe` atKey "12"

    prop "round-trips through the derived index" $ \index ->
      indicesOf (atIndex index) `shouldBe` [if index < 0 then Nothing else Just index]

indicesOf :: JsonPointer -> [Maybe Int]
indicesOf pointer = runPointer pointer (\index _ -> [index])
