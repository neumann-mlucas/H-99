dupli :: [a] -> [a]
dupli (x:xs) = (x:x:dupli xs)
dupli [] = []

main =
  putStrLn $ show $  dupli [1, 2, 3]
