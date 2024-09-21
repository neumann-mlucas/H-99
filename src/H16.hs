dropEvery xs n = [x | (True, x) <- mask' xs]
  where 
  mask' = zip [mod i n /= 0 | i <-[1..]]

drop' :: [a] -> Int -> Int -> [a]
drop' [] _ _ = []
drop' (x:xs) i n = case mod i n of 
  0 ->  drop' xs (i+1) n
  otherwise -> x: (drop' xs (i+1) n)

main = do
  putStrLn $ show $ dropEvery "abcdefghik" 3
  putStrLn $ show $ drop' "abcdefghik" 1 3
