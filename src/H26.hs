import Data.List

combinations :: (Ord a, Eq a) => Int -> [a] -> [[a]]
combinations n xs = sort $ iterate applyExpand (toList xs) !! (n-1)
  where
  expandComb ys = map (\y->y:ys) (deleteAll ys xs)
  applyExpand = concat . map expandComb
  toList = map (\x->x:[])

deleteAll :: (Eq a) => [a] -> [a] -> [a]
deleteAll ys xs = foldl (\x y -> delete y x) xs ys

main = do 
  print $ combinations 3 "abcdef"
