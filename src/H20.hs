removeAt :: [a] -> Int -> [a]
removeAt xs n = (take (n-1) xs) ++ (drop n xs)


main = do
  putStrLn $ show $ removeAt "abcd" 2
