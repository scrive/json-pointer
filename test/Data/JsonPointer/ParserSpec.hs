module Data.JsonPointer.ParserSpec (spec) where

import Data.JsonPointer
import Data.JsonPointer.Gen
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck

keysOf :: JsonPointer -> [T.Text]
keysOf pointer = run pointer (\_ key -> [key])

indicesOf :: JsonPointer -> [Maybe Int]
indicesOf pointer = run pointer (\index _ -> [index])

spec :: Spec
spec = do
  describe "parseJsonPointer" $ do
    it "parses the empty string as the empty pointer" $
      parseJsonPointer "" `shouldBe` Right mempty

    it "parses the reference tokens" $
      keysOf <$> parseJsonPointer "/foo/bar" `shouldBe` Right ["foo", "bar"]

    it "parses an empty reference token" $
      keysOf <$> parseJsonPointer "/" `shouldBe` Right [""]

    it "unescapes ~1 into a slash" $
      keysOf <$> parseJsonPointer "/a~1b" `shouldBe` Right ["a/b"]

    it "unescapes ~0 into a tilde" $
      keysOf <$> parseJsonPointer "/a~0b" `shouldBe` Right ["a~b"]

    it "recognizes a numeric reference token as an index" $
      indicesOf <$> parseJsonPointer "/foo/12" `shouldBe` Right [Nothing, Just 12]

    it "does not treat a reference token with a leading zero as an index" $
      indicesOf <$> parseJsonPointer "/01/0002" `shouldBe` Right [Nothing, Nothing]

    it "keeps a reference token with a leading zero as a key" $
      keysOf <$> parseJsonPointer "/01" `shouldBe` Right ["01"]

    it "recognizes a plain zero as an index" $
      indicesOf <$> parseJsonPointer "/0" `shouldBe` Right [Just 0]

    it "rejects a pointer not starting with a slash" $
      parseJsonPointer "foo" `shouldSatisfy` isLeft

    it "rejects an illegal escape sequence" $
      parseJsonPointer "/a~2b" `shouldSatisfy` isLeft

    it "rejects a trailing tilde" $
      parseJsonPointer "/a~" `shouldSatisfy` isLeft

    prop "round-trips a rendered pointer" $ \(Pointer pointer) ->
      parseJsonPointer (T.pack (show pointer)) `shouldBe` Right pointer

    context "in the relative URI form" $ do
      it "parses a pointer behind a hash" $
        keysOf <$> parseJsonPointer "#/foo/bar" `shouldBe` Right ["foo", "bar"]

      it "parses a bare hash as the empty pointer" $
        parseJsonPointer "#" `shouldBe` Right mempty

      it "parses both forms into the same pointer" $
        parseJsonPointer "#/foo/bar" `shouldBe` parseJsonPointer "/foo/bar"

      it "accepts the hash only as the very first character" $
        parseJsonPointer "##/foo" `shouldSatisfy` isLeft

      it "treats a hash inside a reference token as a plain character" $
        keysOf <$> parseJsonPointer "/foo#/bar" `shouldBe` Right ["foo#", "bar"]

      prop "round-trips a rendered pointer behind a hash" $ \(Pointer pointer) ->
        parseJsonPointer (T.pack ('#' : show pointer)) `shouldBe` Right pointer

    context "URL-decoding" $ do
      it "does not decode a percent-escape in the plain form" $
        keysOf <$> parseJsonPointer "/a%20b" `shouldBe` Right ["a%20b"]

      it "does not decode a percent-escape in the relative URI form" $
        keysOf <$> parseJsonPointer "#/a%20b" `shouldBe` Right ["a%20b"]

      it "does not decode a percent-escaped tilde into an escape sequence" $
        keysOf <$> parseJsonPointer "#/a%7E1b" `shouldBe` Right ["a%7E1b"]

      it "does not decode a percent-escaped slash into a separator" $
        keysOf <$> parseJsonPointer "#/a%2Fb" `shouldBe` Right ["a%2Fb"]

      it "leaves a plus sign alone" $
        keysOf <$> parseJsonPointer "#/a+b" `shouldBe` Right ["a+b"]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
