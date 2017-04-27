module Nine
    ( pack
    ) where

pack :: Eq a => [a] -> [[a]]
pack xs =
  case xs of
    [] ->
      []
    x:xs ->
      (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
