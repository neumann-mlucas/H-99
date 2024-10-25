gcd_ :: Int -> Int -> Int
gcd_ a b 
  | b == 0    = abs a
  | otherwise = gcd_ b (mod a b)

main = do
  print $ gcd_ 36 63
  print $ gcd_ (-3) (-6)
  print $ gcd_ (-3) (-6)
