data Item a = Zero | Single a | Multiple Int a
  deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Item a]
encodeDirect = encode Zero 

encode :: (Eq a) => Item a -> [a] -> [Item a]
encode _ [] = []
encode Zero (x:xs) = encode (Single x) xs
encode (Single a) (x:xs) = case a == x of
  True -> encode (Multiple 2 a) xs
  False -> (Single a) : encode (Single x) xs
encode (Multiple n a) (x:xs) = case a == x of
  True -> encode (Multiple (n+1) a) xs
  False -> (Multiple n a) : encode (Single x) xs


main =
  putStrLn $ show $ encodeDirect "aaaabccaadeeee"
