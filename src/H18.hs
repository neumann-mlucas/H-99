slice :: [a] -> Int -> Int -> [a]
slice xs s e = drop (s-1) . take e $ xs 


main = do
  putStrLn $ show $ slice "abcdefghik" 3 7
