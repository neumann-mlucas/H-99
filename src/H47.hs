import Text.Printf (printf)
import Data.List (intercalate)

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table func =
  [[ a, b, func a b ] | a <- [False, True] , b <- [False, True]]

formatRow [a, b, result] =
  printf "| %-5s | %-5s | %-5s  |" (show a) (show b) (show result)

main :: IO ()
main = do
  putStrLn "+-------+-------+--------+"
  putStrLn "| A     | B     | Result |"
  putStrLn "+-------+-------+--------+"
  putStrLn $ intercalate "\n" $ map formatRow $ table (\a b ->  a `and'` (a `or'` b))
  putStrLn "+-------+-------+--------+"
