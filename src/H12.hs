data Item a = Single a | Multiple Int a
  deriving (Show)

toSeq :: Item a -> [a]
toSeq (Single a) = [a]
toSeq (Multiple n a) = take n $ repeat a

decodeModified = concat . map toSeq 


main =
  putStrLn $ show $ decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
