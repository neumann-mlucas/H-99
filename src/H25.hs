import System.Random
import Data.List

rndPermutation xs = map (xs!!) . take (length xs) . nub . randomRs (0, (length xs) -1) <$> getStdGen

main = do 
  rndPermutation "abcdef" >>= print
