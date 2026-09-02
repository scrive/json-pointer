module Data.JsonPointer.AesonSpec (spec) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.JsonPointer
import Data.JsonPointer.Gen
import Test.Hspec
import Test.Hspec.QuickCheck

document :: Value
document =
  object
    [ "foo" .= object ["bar" .= (1 :: Int), "0" .= ("keyed" :: String)]
    , "list" .= [object ["x" .= True], Null]
    , "empty" .= object ["" .= ("blank" :: String)]
    ]

spec :: Spec
spec = do
  describe "value" $ do
    it "returns the whole document for the empty pointer" $
      value mempty document `shouldBe` Just document

    it "looks up a nested object key" $
      value (atKey "foo" <> atKey "bar") document `shouldBe` Just (Number 1)

    it "indexes into an array" $
      value (atKey "list" <> atIndex 0 <> atKey "x") document `shouldBe` Just (Bool True)

    it "looks up an empty key" $
      value (atKey "empty" <> atKey "") document `shouldBe` Just (String "blank")

    it "returns a null stored in the document" $
      value (atKey "list" <> atIndex 1) document `shouldBe` Just Null

    it "uses the textual key when indexing an object" $
      value (atKey "foo" <> atIndex 0) document `shouldBe` Just (String "keyed")

    it "indexes into an array through a numeric key" $
      value (atKey "list" <> atKey "0" <> atKey "x") document `shouldBe` Just (Bool True)

    it "resolves pointers that compare equal alike" $
      value (atKey "list" <> atKey "0") document
        `shouldBe` value (atKey "list" <> atIndex 0) document

    it "returns Nothing for a missing key" $
      value (atKey "nope") document `shouldBe` Nothing

    it "returns Nothing for an out of bounds index" $
      value (atKey "list" <> atIndex 7) document `shouldBe` Nothing

    it "returns Nothing when indexing an array with a non-numeric key" $
      value (atKey "list" <> atKey "x") document `shouldBe` Nothing

    it "applies the reference tokens left to right" $
      value (atKey "bar" <> atKey "foo") document `shouldBe` Nothing

    it "returns Nothing when descending into a scalar" $
      value (atKey "foo" <> atKey "bar" <> atKey "deeper") document `shouldBe` Nothing

  describe "nullableValue" $ do
    it "behaves like value on a hit" $
      nullableValue (atKey "foo" <> atKey "bar") document `shouldBe` Number 1

    it "returns Null on a miss" $
      nullableValue (atKey "nope") document `shouldBe` Null

  describe "ToJSON" $ do
    it "renders the plain form" $
      toJSON (atKey "foo" <> atIndex 0) `shouldBe` String "/foo/0"

    it "renders the empty pointer as the empty string" $
      toJSON (mempty @JsonPointer) `shouldBe` String ""

    it "escapes the reference tokens" $
      toJSON (atKey "a/b") `shouldBe` String "/a~1b"

  describe "FromJSON" $ do
    it "parses the plain form" $
      fromJSON (String "/foo/0") `shouldBe` Success (atKey "foo" <> atIndex 0)

    it "parses the relative URI form" $
      fromJSON (String "#/foo/0") `shouldBe` Success (atKey "foo" <> atIndex 0)

    it "does not URL-decode" $
      fromJSON (String "#/a%2Fb") `shouldBe` Success (atKey "a%2Fb")

    it "fails on an illegal escape sequence" $
      fromJSON @JsonPointer (String "/a~2b") `shouldSatisfy` isError

    it "fails on a non-string" $
      fromJSON @JsonPointer (Number 1) `shouldSatisfy` isError

    prop "round-trips a pointer through ToJSON" $ \(Pointer pointer) ->
      fromJSON (toJSON pointer) `shouldBe` Success pointer

isError :: Result a -> Bool
isError = \case
  Error _ -> True
  Success _ -> False
