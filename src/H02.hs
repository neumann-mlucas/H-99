lastButOne :: [a] -> a
lastButOne  [] = error "empty list"
lastButOne [x] = error "one element list"
lastButOne (x:y:[]) = x
lastButOne (_:xs) = lastButOne xs


main = 
  putStrLn $ show $ lastButOne ['a'..'z']
