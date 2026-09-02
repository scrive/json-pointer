module Data.JsonPointer.AesonSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.JsonPointer
import Test.Hspec

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
