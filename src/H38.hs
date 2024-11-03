import Data.List
import Data.Time

gcd_ :: Int -> Int -> Int
gcd_ a b 
  | b == 0    = abs a
  | otherwise = gcd_ b (mod a b)

coprime :: Int -> Int -> Bool
coprime n m = (==1) $ gcd_ n m

totient :: Int -> Int
totient n = length $ filter (coprime n ) [1..n-1]


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

phi = product . map (\(m, p) -> (p-1) * p ^ (m-1)) . primeFactorsMult

main = do
  start <- getCurrentTime
  print $ totient 10090
  stop <- getCurrentTime
  print $ diffUTCTime stop start

  start <- getCurrentTime
  print $ phi 10090
  stop <- getCurrentTime
  print $ diffUTCTime stop start
