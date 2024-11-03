gcd_ :: Int -> Int -> Int
gcd_ a b 
  | b == 0    = abs a
  | otherwise = gcd_ b (mod a b)

coprime :: Int -> Int -> Bool
coprime n m = (==1) $ gcd_ n m

main = do
  print $ coprime 35 64
