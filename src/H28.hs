import Data.List

lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort = concat . lsort. groupBy equalLength . lsort 
  where 
  equalLength xs ys = (length xs) == (length ys)


main = do
  print $ lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  print $ lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
