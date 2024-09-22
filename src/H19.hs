rotate :: [a] -> Int -> [a]
rotate xs n = (drop n' xs) ++ (take n' xs) 
  where
  n' = mod n (length xs)


main = do
  putStrLn $ show $ rotate "abcdefgh" 3
  putStrLn $ show $ rotate "abcdefgh" (-2)
