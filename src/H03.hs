elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (pred n)
elementAt [] _ = error "out of bounds"


main = 
  putStrLn $ show $ elementAt "haskell" 5
