import Text.Printf (printf)
import Data.List (intercalate)
import Control.Monad (replicateM)

and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

equ' :: Bool -> Bool -> Bool
equ' True  True  = True
equ' False False = True
equ' _     _     = False

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n func =
  [a ++ [func a] | a <- replicateM n [True, False]]

formatRow :: (Show a) => [a] -> String
formatRow xs =
  "|" ++ (intercalate "" $ map (printf " %-5s |" . show) xs)

formatHeader :: (Show a) => [a] -> String
formatHeader xs =
  "+" ++ (intercalate "" $ Prelude.replicate (length xs) "-------+")

main :: IO ()
main = do
  let h1 = ["A", "B", "R"]
  putStrLn $ intercalate "\n" $ [formatHeader h1, formatRow h1, formatHeader h1]
  putStrLn $ intercalate "\n" $ map formatRow $ tablen 2 (\[a,b] ->  a `and'` (a `or'` b))
  putStrLn $ formatHeader h1
  putStrLn ""

  let h2 = ["A", "B", "C", "R"]
  putStrLn $ intercalate "\n" $ [formatHeader h2, formatRow h2, formatHeader h2]
  putStrLn $ intercalate "\n" $ map formatRow $ tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
  putStrLn $ formatHeader h2
