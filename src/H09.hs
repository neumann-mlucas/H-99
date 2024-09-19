pack xs = pack' "" xs

pack' :: [Char] -> [Char] -> [[Char]]
pack' acc [] = [acc]
pack' "" (x:xs) = pack' [x] (x:xs)
pack' acc (x:xs) = case (head acc) == x of
  True -> pack' (x:acc) xs
  False -> [acc] ++ (pack' "" xs)


main =
  putStrLn $ show $ pack "aaaabccaadeeee" 
