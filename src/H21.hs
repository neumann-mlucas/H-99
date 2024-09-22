insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = (take (n-1) xs) ++ [x] ++ (drop (n-1) xs)


main = do
  putStrLn $ show $ insertAt 'X' "abcd" 2
