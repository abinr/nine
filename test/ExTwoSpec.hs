module ExTwoSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

main :: IO ()
main = do hspec spec

spec :: Spec
spec = do
  describe "next to last as Maybe" $ do
    it "should return first when length is 2" $ do
      nextToLastAsMaybe [4, 5] `shouldBe` Just 4
    it "should be polymorphic" $ do
      nextToLastAsMaybe ['a'..'e'] `shouldBe` Just 'd'
    it "should return Nothing when length is 1" $ do
      nextToLastAsMaybe [9] `shouldBe` Nothing
    it "should return Nothing when length is 0" $ do
      nextToLastAsMaybe ([]::[Int]) `shouldBe` Nothing
    it "should return Maybe for arbitray list" $
      property $ \xs -> nextToLastAsMaybe xs == f (xs :: [Int])

f = fst . foldl (\(a,b) x -> (b, Just x)) (Nothing, Nothing)

nextToLastAsMaybe :: [a] -> Maybe a
nextToLastAsMaybe list =
  case list of
    [] ->
      Nothing
    [x] ->
      Nothing
    x : y : [] ->
      Just x
    _:xs ->
      nextToLastAsMaybe xs


