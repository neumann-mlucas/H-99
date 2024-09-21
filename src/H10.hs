pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (x==) xs): pack (dropWhile (x==) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- pack xs]

main =
  putStrLn $ show $ encode "aaaabccaadeeee" 
