isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all ((/=0).(mod n)) [2..n-1]

primesR n m = filter isPrime [n..m]

main = 
  print $ primesR 10 20

