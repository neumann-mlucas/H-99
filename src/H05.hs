myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

main = 
  putStrLn $ show $ myReverse [1,2,3,4]
