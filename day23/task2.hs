module Main where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Map

type Cup = Int
type CurrCup = Int
type Cups = M.Map Int Int

len = 1000000
zipC = [0..len]

playGame :: (CurrCup, Cups) -> (CurrCup, Cups)

playGame (curr, cups) = (cups'' ! curr, cups'')
  where cup1 = cups ! curr
        cup2 = cups ! cup1
        cup3 = cups ! cup2
        cupN = cups ! cup3
        cups' = M.insert curr cupN cups
        destCup = head $ [ x | x <- [curr-1,curr-2..1] ++ [len,len-1..1], x /= cup1, x/=cup2,x/=cup3 ]
        m = cups' ! destCup
        cups'' = M.insert cup3 m $ M.insert cup2 cup3 $ M.insert cup1 cup2 $ M.insert destCup cup1 cups'
 
applyN :: (CurrCup, Cups) -> Int -> (CurrCup, Cups)
applyN xs 0 = xs
applyN xs depth = applyN (playGame xs) (depth -1)

main :: IO ()
main = do
  let array = [9,6,2,7,1,3,8,5,4] ++ [10..len]
  let current = 9
  let m' = M.fromList $ zip array (tail array) ++ [(last array, head array)]

  let result = snd $ applyN (current, m') 10000000
  --print result
  let r1 = result ! 1
  let r2 = result ! r1
  print $ [r1, r2, r1*r2]
