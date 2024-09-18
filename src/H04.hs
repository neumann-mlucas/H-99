myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0 

main = 
  putStrLn $ show $ myLength "Hello, world!"
