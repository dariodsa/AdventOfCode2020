{-# LANGUAGE OverloadedStrings #-}
module Main where
 
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (groupBy, sort, nub)
 
type Ingredient = T.Text
type Alergen = T.Text
 
getList :: [String] -> [([Ingredient], [Alergen])]
getList [] = []
getList (x':xs) = [res] ++ getList xs
  where
    x = T.pack x'
    res = (ings, algs)
    ings = T.splitOn " " ((T.splitOn " (" x) !! 0)
    algs = T.splitOn ", " algs_temp
    algs_temp = T.init $ (T.splitOn " (contains " x) !! 1
 
getIng :: [String] -> [Ingredient]
getIng [] = []
getIng (x':xs) = ings ++ getIng xs
  where
    x = T.pack x'
    ings = T.splitOn " " ((T.splitOn " (" x) !! 0)
   
numTimesFound :: Ord a => a -> [a] -> Integer
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list
 
--generateDict :: [([Ingredient], [Alergen])] -> [(Alergen, [Ingredient])]
generateDict xs = M.fromList l2
  where
    l1 = sort $ [ (alg, ings) | (ings, algs) <- xs, alg <- algs]
    groups = groupBy (\x y -> fst x == fst y) l1
    l2 = [(alg, sort ings) | group' <- groups, let alg = fst (group' !! 0), let ings = (concat $ map snd group')]
   
 
main = do
   rs <- readFile "input.txt"
   let xs = lines rs
   let list = getList xs
   let dict = generateDict list
   -- M.Map Alergen [Ingredient]
  
   let list = M.toDescList dict
   --let forbidden = getForb list
  
   let list' = map (\(alg, ings) -> (alg, take 5 $ reverse $ nub $ sort $ [ (numTimesFound ing ings, ing) | ing <- ings] )) list
   let intgs = getIng xs
   print $ length $ filter (\x ->  (x /= "lgllb")
                                && (x /= "rmd")
                                && (x /= "mdbq")
                                && (x /= "jdggtft")
                                && (x /= "cdvjp")
                                && (x /= "bdnrnx")
                                && (x /= "ztdctgq")
                                && (x /= "xgtj")
                                ) intgs
  
   print $ list'
   print "Done."
