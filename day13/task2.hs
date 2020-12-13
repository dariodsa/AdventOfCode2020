module Main where

import Data.List.Split (splitOn)
import Data.List (sort)

type BusId = Integer

getBuses :: [Char] -> [(Integer, BusId)]
getBuses xs = read_to_int $ remove_x $ split_xs xs
   where split_xs xs = zip [1..] $ splitOn "," xs
         remove_x = filter $ (/=) "x" . snd
         read_to_int = map read_bus

         read_bus :: (Integer, String) -> (Integer, BusId)
         read_bus (id, x) = (read x, (id - 1) * (-1))
         
getTime :: [(BusId, Integer)] -> (Integer, Integer)
getTime ((n1, a1):(n2, a2):[]) = (,) (n1*n2) $ head $ dropWhile (\f->((f - a2) `mod` n2 /= 0)) $ [a1,a1+n1..]
getTime ((n1, a1):xs) = (,) (n*n1) $ head $ dropWhile (\f->( (f - a1 ) `mod` n1 /= 0)) $ [x,x+n..]
   where
     sol = getTime xs
     n = fst sol
     x = snd sol

main = do 
   rs <- readFile "input.txt"
   let xs = lines rs
   let timeDepart = read  (head xs) :: Integer
   let buses = reverse $ sort $ getBuses (xs !! 1)
   print buses
   let result = getTime buses 
   print result
