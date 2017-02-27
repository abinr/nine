module ExSevenSpec (main, spec) where

import Test.Hspec

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "Flatten should merge list of lists into one list" $ do
    it "Should flatten an empty list into an empty list" $ do
      flatten (List []) `shouldBe` ([]::[Int])
    it "Should flatten a single value into a 1 element list" $ do
      flatten (Elem 5) `shouldBe` [5]
    it "Should flatten a Nested List" $ do
      flatten t `shouldBe` [1, 2, 3, 4, 5]


t = (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

data NestedList a
  = Elem a
  | List [NestedList a]
  deriving (Eq, Show)

flatten :: NestedList a -> [a]
flatten nxs =
  case nxs of
    Elem n ->
      [n]
    List [] ->
      []
    List (x:xs) ->
      flatten x ++ flatten (List xs)
