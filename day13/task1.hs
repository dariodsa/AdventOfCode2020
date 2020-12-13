module Main where

import Data.List.Split (splitOn)

type BusId = Int

getBuses :: [Char] -> [BusId]
getBuses = read_to_int . remove_x . split_xs
   where split_xs = splitOn ","
         remove_x = filter $ (/=) "x"
         read_to_int = map read
         
findEariestBus :: Int -> [BusId] -> (BusId, Int)
findEariestBus timeDepart buses = minimum $ zip buses_time buses
    where
      buses_time = map (getTimeBus timeDepart) buses
getTimeBus :: Int -> Int -> Int
getTimeBus timeDepart bus = ceiling ( timeDepart_f /  bus_f) * bus
  where
    timeDepart_f = fromIntegral timeDepart
    bus_f = fromIntegral bus

main = do 
   rs <- readFile "input.txt"
   let xs = lines rs
   let timeDepart = read  (head xs) :: Int
   let buses = getBuses (xs !! 1)
   let result = findEariestBus timeDepart buses
   let bus_id = snd result
   let time = fst result
   print $ (time - timeDepart) * bus_id
