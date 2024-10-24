combination :: Int -> [a] -> [[a]]
combination 0 xs     = [[]]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
  ts = [x:ys | ys <- combination (n-1) xs] 
  ds = [ys | ys <- combination n xs]

main = do 
  print . length $ combination 3 "abcdef"
