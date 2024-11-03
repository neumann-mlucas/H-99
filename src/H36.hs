import Data.List

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0).(mod n)) [2..n-1]

primeFactor :: Int -> [Int]
primeFactor n = helper n (filter isPrime [2..])
  where
  helper 1 _ = []
  helper n (p:ps)
    | mod n p == 0 = p:(helper (div n p) (p:ps))
    | otherwise  = helper n ps

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map (\x->(length x, head x)) . group . primeFactor

main = do
  print $ primeFactorsMult 315
