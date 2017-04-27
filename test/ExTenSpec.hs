module ExTenSpec where

import Test.Hspec
import Nine (pack)

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Run length encode a list" $ do
    it "should return an empty list for empty list" $ do
      encode [] `shouldBe` ([] :: [(Int, Char)])
    it "should return (1, a) for a single element" $ do
      encode [1] `shouldBe` [(1, 1)]
    it "should run length code" $ do
      encode "aaaabccaadeeee"
        `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: Eq a => [a] -> [(Int, a)]
encode =
  fmap ((,) <$> length <*> head) . pack
