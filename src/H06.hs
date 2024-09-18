isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

main = do
  putStrLn $ show $ isPalindrome "madamimadam"
  putStrLn $ show $ isPalindrome [1,2,4,8,16,8,4,2,1]
