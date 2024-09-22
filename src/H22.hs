range :: Int -> Int -> [Int]
range s e = [s..e]

range' start end 
  | start == end = [start]
  | start < end = start:(range' (start+1) end)
  | start > end = start:(range' (start-1) end)


main = do
  putStrLn $ show $ range 4 9
  putStrLn $ show $ range' 4 9
