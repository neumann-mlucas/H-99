compress :: (Eq a) => [a] -> [a]
compress = compress' ' '

compress' :: (Eq a) => a -> [a] -> [a]
compress' p (x:xs) = case p == x of
  True -> compress' x xs
  False -> [x] ++ compress' x xs
compress' _ [] = []


main =
  putStrLn $ show $ compress "aaaabccaadeeee" 
