isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0).(mod n)) [2..n-1]

goldbach :: Int -> (Int, Int)
goldbach n = head pairs 
  where
    primes = filter isPrime [1..n]
    pairs = [(x,y) | x <- primes, y <- primes, x+y == n] 

main = 
  print $ goldbach 28
