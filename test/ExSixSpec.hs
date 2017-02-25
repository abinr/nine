module ExSixSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative

main :: IO ()
main =
  hspec spec


spec :: Spec
spec = do
  describe "a palindrome checker" $ do
    it "should recognize a palindrome" $ do
      isPalindrome "madamimadam" `shouldBe` True
    it "should recognize a non palindrome" $ do
      isPalindrome [1,2,3] `shouldBe` False
    it "should recognize a long palindrome" $ do
      isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True
    it "should recognize correctly for arbitrary list" $
      property $ \xs -> isPalindrome xs == f (xs :: [Char])

f :: (Eq a) => [a] -> Bool
f = (==) <*> reverse

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list =
  list == reverse list
