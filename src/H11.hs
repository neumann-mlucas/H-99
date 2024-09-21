data Item a = Single a | Multiple Int a
  deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (x==) xs): pack (dropWhile (x==) xs)

encodeModified :: (Eq a) => [a] -> [Item a]
encodeModified  xs = [toItem x | x <- pack xs]

toItem :: (Eq a) => [a] -> Item a
toItem xs = case length xs of 
  1 -> Single (head xs)
  _ -> Multiple (length xs) (head xs)


main =
  putStrLn $ show $ encodeModified "aaaabccaadeeee"
