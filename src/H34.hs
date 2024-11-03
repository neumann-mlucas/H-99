gcd_ :: Int -> Int -> Int
gcd_ a b 
  | b == 0    = abs a
  | otherwise = gcd_ b (mod a b)

coprime :: Int -> Int -> Bool
coprime n m = (==1) $ gcd_ n m

totient :: Int -> Int
totient n = length $ filter (coprime n ) [1..n-1]

main = do
  print $ totient 10
  print $ totient 315
