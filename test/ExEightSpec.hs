module ExEightSpec (main, spec) where

import Test.Hspec

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "Eliminate consecutive duplicates" $ do
    it "Should return identity on empty list" $ do
      compress [] `shouldBe` ([] :: [Int])
    it "Should return identity on single elment list" $ do
      compress ['a'] `shouldBe` ['a']
    it "Should return identity on list no dupes" $ do
      compress [1, 2, 3] `shouldBe` [1,2,3]
    it "Should eliminate dupes" $ do
      compress ['a', 'a'] `shouldBe` ['a']
    it "Should eliminate dupes for complicated list" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))
  | x == y = compress ys
  | otherwise = x : compress ys
compress list = list

