isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0).(mod n)) [2..n-1]

primeFactor n = helper n (filter isPrime [2..])
  where
  helper 1 _ = []
  helper n (p:ps)
    | mod n p == 0 = p:(helper (div n p) (p:ps))
    | otherwise  = helper n ps

main = do
  print $ primeFactor 315
