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
  describe "jsonPointer" $ do
    it "parses the empty string as the empty pointer" $
      parse jsonPointer "" `shouldBe` Right mempty

    it "parses the reference tokens" $
      keysOf <$> parse jsonPointer "/foo/bar" `shouldBe` Right ["foo", "bar"]

    it "parses an empty reference token" $
      keysOf <$> parse jsonPointer "/" `shouldBe` Right [""]

    it "unescapes ~1 into a slash" $
      keysOf <$> parse jsonPointer "/a~1b" `shouldBe` Right ["a/b"]

    it "unescapes ~0 into a tilde" $
      keysOf <$> parse jsonPointer "/a~0b" `shouldBe` Right ["a~b"]

    it "recognizes a numeric reference token as an index" $
      indicesOf <$> parse jsonPointer "/foo/12" `shouldBe` Right [Nothing, Just 12]

    it "rejects a pointer not starting with a slash" $
      parse jsonPointer "foo" `shouldSatisfy` isLeft

    it "rejects an illegal escape sequence" $
      parse jsonPointer "/a~2b" `shouldSatisfy` isLeft

    it "rejects a trailing tilde" $
      parse jsonPointer "/a~" `shouldSatisfy` isLeft

    prop "round-trips a rendered pointer" $ \(Pointer pointer) ->
      parse jsonPointer (T.pack (show pointer)) `shouldBe` Right pointer

  describe "jsonPointerUriFragment" $ do
    it "parses a pointer behind a hash" $
      keysOf <$> parse jsonPointerUriFragment "#/foo/bar" `shouldBe` Right ["foo", "bar"]

    it "parses a bare hash as the empty pointer" $
      parse jsonPointerUriFragment "#" `shouldBe` Right mempty

    it "requires the hash" $
      parse jsonPointerUriFragment "/foo" `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
