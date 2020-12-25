module Main where

import Data.List (foldl')

type Cup = Int
type CurrCup = Int
type Cups = [Cup]

len = 1000000
zipC = [0..len]

insert3Cup :: Cups -> Cup -> Cups -> Cups
--insert3Cup _ _ [] = []
insert3Cup cups3 desC xs = concat [f x' | x' <- xs]
 where
  f x | x == desC = x : cups3
      | otherwise = [x]
 
playGame' :: ((CurrCup, Int), Cups) -> ((CurrCup, Int), Cups)
playGame' ((c,i),xs) = ((i,c),filter (>30) xs)

playGame :: ((CurrCup, Int), Cups) -> ((CurrCup, Int), Cups)
playGame ((curr,currId), xs) = ((curr',(currId' + 1) `mod` len), xs'')
  where
   
    --currId = fst $ head $ filter (\x -> snd x == curr) (zip zipC xs)
    --curr = xs !! currId
    threeCups = [ xs !! ((currId + id) `rem` len) | id <-[1..3]]
    d' = [ x | x <- xs, x < curr, x /= head threeCups, x /= threeCups !! 1, x /= threeCups !! 2 ]
    xs' = filter (\x -> x /= head threeCups && x /= threeCups !! 1 && x/= threeCups !! 2) xs
    maxCup = foldl' max 0 [ x | x <- xs', x > curr]
    destinationCup = if null (take 1 d') then maxCup
                     else foldl' max 0 d'
    desId = fst $ head $ filter (\x -> snd x == destinationCup) (zip zipC xs')
    
    --fp = (takeWhile (/=destinationCup) xs' ) ++ [destinationCup]
    --sp = tail $ takeFrom (==destinationCup) xs'
    --xs'' = fp ++ threeCups ++ sp
    xs'' = seq xs' $ insert3Cup threeCups destinationCup xs'
    currId' = fst $ head $ filter (\x -> snd x == curr) (zip zipC xs'')
    curr' = xs'' !! ((currId' + 1) `rem` len)
 
applyN :: ((CurrCup, Int), Cups) -> Int -> ((CurrCup, Int), Cups)
applyN xs 10000000 = xs
applyN xs depth = applyN (playGame' xs) (depth +1)

main :: IO ()
main = do
  let array = [3,8,9,1,2,5,4,6,7] ++ [10..len]
  let current = 3
  let result = snd $ applyN ((current, 0), array) 1
  let id = fst $ head $ filter (\x -> snd x == 1) (zip [0..] result)
  print $ result !! ((id+1) `mod` len)
  print $ result !! ((id+2) `mod` len)
