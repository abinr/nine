module ExFiveSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reverse a list" $ do
    it "should equal itself after reversing twice" $
      property $ \xs -> (myReverse . myReverse) xs == (xs :: [Int]) 
    it "should be polymorphic" $
      property $ \xs -> (myReverse . myReverse) xs == (xs :: [Char]) 

myReverse :: [a] -> [a]
myReverse list =
  case list of
    [] ->
      []
    x:xs ->
      (myReverse xs) ++ [x]
