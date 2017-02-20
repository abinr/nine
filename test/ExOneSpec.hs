module ExOneSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "last as Maybe" $ do
    it "should return the last element of a list" $ do
      lastAsMaybe [0..5] `shouldBe` Just 5
    it "should be polymorphic" $ do
      lastAsMaybe ['a'..'d'] `shouldBe` Just 'd'
    it "should return Nothing for an empty list" $ do
      lastAsMaybe ([] :: [Int]) `shouldBe` Nothing
    it "should return a Maybe result for an arbitrary list" $
      property $ \xs -> lastAsMaybe xs == f (xs :: [Int])

f = (fmap fst) . uncons . reverse

lastAsMaybe :: [a] -> Maybe a
lastAsMaybe list =
  case list of
    [] ->
      Nothing
    [x] ->
      Just x
    x:xs ->
      lastAsMaybe xs
