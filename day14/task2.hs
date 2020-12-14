{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M

binary_length = 36

data Command = Mask String
             | Memory Integer Integer
        deriving (Show, Read)

binToDecimal :: String -> Integer
binToDecimal = foldl (\ac x-> ac * 2 + f x) 0
   where
     f '0' = 0
     f '1' = 1

decimalToBin :: Integer -> String
decimalToBin 0 = "0"
decimalToBin x | x `mod` 2 == 0 = decimalToBin (x `div` 2) ++ "0"
               | x `mod` 2 == 1 = decimalToBin (x `div` 2) ++ "1"

addPrefix' :: Int -> String -> String
addPrefix' len xs = concat (take (len - length xs) $ repeat "0") ++ xs
addPrefix = addPrefix' binary_length 

process :: [String] -> [Command]
process (x:xs) | "mask" `isPrefixOf` x = [extractMask x] ++ process xs
               | "mem" `isPrefixOf` x = [extractMem x] ++ process xs
  where extractMask x = Mask $ splitOn " = " x !! 1
        extractMem x = Memory address value
          where
            value = read $ splitOn " = " x !! 1
            address = read $ splitOn "]"  (splitOn "[" x !! 1) !! 0
process [] = []
          

calculate :: String -> String -> String
calculate ('0':xs1) ('0':xs2) = "0" ++ calculate xs1 xs2
calculate ('0':xs1) ('1':xs2) = "1" ++ calculate xs1 xs2
calculate ('1':xs1) ('0':xs2) = "1" ++ calculate xs1 xs2
calculate ('1':xs1) ('1':xs2) = "1" ++ calculate xs1 xs2
calculate ('X':xs1) ('0':xs2) = "X" ++ calculate xs1 xs2
calculate ('X':xs1) ('1':xs2) = "X" ++ calculate xs1 xs2
calculate "" "" = ""


run :: [Command] -> [(Integer, Integer)]
run = runInternal ""
  where runInternal :: String -> [Command] -> [(Integer, Integer)]
        runInternal _ [] = []
        runInternal _ (Mask mask:xs) = runInternal mask xs
        runInternal mask ((Memory address value):xs) = results ++ runInternal mask xs
           where result = calculate mask $ addPrefix $ decimalToBin address
                 results = getAll result value         
        
getAll :: String -> Integer -> [(Integer, Integer)]
getAll xs num = map (, num) addresses
  where
    addresses = map binToDecimal $ get' xs
    get' :: String -> [String]
    get' "0" = ["0"]
    get' "1" = ["1"]
    get' "X" = ["0", "1"]
    get' ('X':xs) = ans0 ++ ans1
      where ans0 = map ((++) "0") other
            ans1 = map ((++) "1") other
            other = get' xs
    get' (x:xs) = map ((++) [x]) other
      where other = get' xs
      



main = do 
   rs <- readFile "input.txt"
   let xs = lines rs
   let commands = process xs
   --print commands
   let res = run commands
   --let values = map (\(x, y) -> (x, binToDecimal y)) res
   let dict = M.fromList res
   let sum = foldl (\acc x -> acc + x) 0 dict
   print sum