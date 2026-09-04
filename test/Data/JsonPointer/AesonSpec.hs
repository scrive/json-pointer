module Data.JsonPointer.AesonSpec (spec) where

import Data.Aeson (Result (..), Value (..), fromJSON, object, toJSON, (.=))
import Data.JsonPointer
import Data.JsonPointer.Gen
import Data.Map.Strict qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck

document :: Value
document =
  object
    [ "foo" .= object ["bar" .= (1 :: Int), "0" .= ("keyed" :: String), "-1" .= ("negative" :: String)]
    , "list" .= [object ["x" .= True], Null]
    , "" .= ("blank" :: String)
    ]

spec :: Spec
spec = do
  describe "pointTo" $ do
    it "returns the whole document for the empty pointer" $
      pointTo mempty document `shouldBe` Just document

    it "looks up a nested object key" $
      pointTo (atKey "foo" <> atKey "bar") document `shouldBe` Just (Number 1)

    it "indexes into an array" $
      pointTo (atKey "list" <> atIndex 0 <> atKey "x") document `shouldBe` Just (Bool True)

    it "looks up an empty key" $
      pointTo (atKey "") document `shouldBe` Just (String "blank")

    it "returns a null stored in the document" $
      pointTo (atKey "list" <> atIndex 1) document `shouldBe` Just Null

    it "uses the textual key when indexing an object" $
      pointTo (atKey "foo" <> atIndex 0) document `shouldBe` Just (String "keyed")

    it "indexes into an array through a numeric key" $
      pointTo (atKey "list" <> atKey "0" <> atKey "x") document `shouldBe` Just (Bool True)

    it "resolves pointers that compare equal alike" $
      pointTo (atKey "list" <> atKey "0") document
        `shouldBe` pointTo (atKey "list" <> atIndex 0) document

    it "uses the textual key for a negative index" $
      pointTo (atKey "foo" <> atIndex (-1)) document `shouldBe` Just (String "negative")

    it "returns Nothing for a negative index into an array" $
      pointTo (atKey "list" <> atIndex (-1)) document `shouldBe` Nothing

    it "returns Nothing for a missing key" $
      pointTo (atKey "nope") document `shouldBe` Nothing

    it "returns Nothing for an out of bounds index" $
      pointTo (atKey "list" <> atIndex 7) document `shouldBe` Nothing

    it "returns Nothing when indexing an array with a non-numeric key" $
      pointTo (atKey "list" <> atKey "x") document `shouldBe` Nothing

    it "applies the reference tokens left to right" $
      pointTo (atKey "bar" <> atKey "foo") document `shouldBe` Nothing

    it "returns Nothing when descending into a scalar" $
      pointTo (atKey "foo" <> atKey "bar" <> atKey "deeper") document `shouldBe` Nothing

  describe "pointToNullable" $ do
    it "behaves like pointTo on a hit" $
      pointToNullable (atKey "foo" <> atKey "bar") document `shouldBe` Number 1

    it "returns Null on a miss" $
      pointToNullable (atKey "nope") document `shouldBe` Null

  describe "ToJSON" $ do
    it "renders the plain form" $
      toJSON (atKey "foo" <> atIndex 0) `shouldBe` String "/foo/0"

    it "renders the empty pointer as the empty string" $
      toJSON (mempty @JsonPointer) `shouldBe` String ""

    it "escapes the reference tokens" $
      toJSON (atKey "a/b") `shouldBe` String "/a~1b"

  describe "ToJSONKey" $ do
    it "renders the plain form as an object key" $
      toJSON (Map.singleton (atKey "foo" <> atIndex 0) (1 :: Int))
        `shouldBe` object ["/foo/0" .= (1 :: Int)]

    it "renders the empty pointer as the empty key" $
      toJSON (Map.singleton (mempty @JsonPointer) (1 :: Int))
        `shouldBe` object ["" .= (1 :: Int)]

    it "escapes the reference tokens" $
      toJSON (Map.singleton (atKey "a/b") (1 :: Int))
        `shouldBe` object ["/a~1b" .= (1 :: Int)]

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

  describe "FromJSONKey" $ do
    it "parses the plain form from an object key" $
      fromJSON (object ["/foo/0" .= (1 :: Int)])
        `shouldBe` Success (Map.singleton (atKey "foo" <> atIndex 0) (1 :: Int))

    it "parses the relative URI form from an object key" $
      fromJSON (object ["#/foo/0" .= (1 :: Int)])
        `shouldBe` Success (Map.singleton (atKey "foo" <> atIndex 0) (1 :: Int))

    it "parses the empty key as the empty pointer" $
      fromJSON (object ["" .= (1 :: Int)])
        `shouldBe` Success (Map.singleton (mempty @JsonPointer) (1 :: Int))

    it "fails on an illegal escape sequence" $
      fromJSON @(Map.Map JsonPointer Int) (object ["/a~2b" .= (1 :: Int)])
        `shouldSatisfy` isError

    prop "round-trips a pointer through ToJSONKey" $ \(Pointer pointer) ->
      fromJSON (toJSON (Map.singleton pointer (1 :: Int)))
        `shouldBe` Success (Map.singleton pointer (1 :: Int))

isError :: Result a -> Bool
isError = \case
  Error _ -> True
  Success _ -> False
