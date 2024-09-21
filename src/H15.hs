repli :: [a] -> Int -> [a]

repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n


main =
  putStrLn $ show $  repli "abc" 3
