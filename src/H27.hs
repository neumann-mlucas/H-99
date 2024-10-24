import Data.List as List

combination :: Int -> [a] -> [[a]]
combination 0 xs     = [[]]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
  ts = [x:ys | ys <- combination (n-1) xs] 
  ds = [ys | ys <- combination n xs]

deleteAll :: (Eq a) => [a] -> [a] -> [a]
deleteAll ys xs = foldl (\x y -> delete y x) xs ys

group_ :: (Ord a) => [Int] -> [a] -> [[[a]]]
group_ [] _ = [[]]
group_ (n:ns) xs = [(g:gs) | g <- combination n xs, gs <- group_ ns (deleteAll g xs)]

main = do 
  mapM_ print $ group_ [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
