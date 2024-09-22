import System.Random
import Data.List

rndSelect :: Int -> Int -> IO [Int]
rndSelect n m = take n . nub . randomRs (1, m) <$> getStdGen

main = do 
  rndSelect 6 49 >>= print
