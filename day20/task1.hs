{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (splitOn, pack, unpack)
import Data.List (sort)

type Id = Int
type Tile = (Id, String, String, String, String)

data Rotation = DEG_0 
              | DEG_90 
              | DEG_180 
              | DEG_270
    deriving (Show, Ord, Eq, Enum)

data Flip = FLIP
          | NO_FLIP
    deriving (Show, Ord, Eq, Enum)

data Side = LeftS
          | RightS
          | BottomS
          | UpS
    deriving (Show, Ord, Eq, Enum)

flipT :: Tile -> Tile
flipT (id, x1, x2, x3, x4) = (id, x3, x4, x1, x2)

apply :: Rotation -> Flip -> Tile -> Tile

apply DEG_0 NO_FLIP t = t
apply DEG_90 NO_FLIP t@(id, x1, x2, x3, x4) = (id, reverse x4, x1, reverse x2, x3)
apply DEG_180 NO_FLIP t = apply DEG_90 NO_FLIP $ apply DEG_90 NO_FLIP t
apply DEG_270 NO_FLIP t = apply DEG_90 NO_FLIP $ apply DEG_180 NO_FLIP t

apply DEG_0 FLIP t = flipT t
apply DEG_90 FLIP t@(id, x1, x2, x3, x4) = flipT (id, reverse x4, x1, reverse x2, x3)
apply DEG_180 FLIP t = flipT $ apply DEG_90 NO_FLIP $ apply DEG_90 NO_FLIP t
apply DEG_270 FLIP t = flipT $ apply DEG_90 NO_FLIP $ apply DEG_180 NO_FLIP t



parse :: [String] -> [Tile]
parse [] = []
parse (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:_:xs) = [tile] ++ parse xs
  where
    tile = (id, x1', x2', x3', x4')
    id = read $ unpack $ splitOn ":" (splitOn " " (pack x0) !! 1) !! 0
    m = [x1] ++ [x2] ++ [x3] ++ [x4] ++ [x5] ++ [x6] ++ [x7] ++ [x8] ++ [x9] ++ [x10]
    x1' = x1
    x3' = x10
    x2' = [c !! 9 | c <- m]
    x4' = [c !! 0 | c <- m]


generate :: Tile -> [Tile]
generate tile = [ apply r f tile | r <- [DEG_0 ..], f <- [FLIP ..] ]

match :: Tile -> Tile -> Side -> Bool
match tile1 tile2 side = (not . null) $ filter (==True) [ f' t1 tile2 side | t1 <- generate tile1]
  where
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) LeftS = x2 == y4
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) RightS = x4 == y2
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) BottomS = x1 == y3
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) UpS = x3 == y1

getTileID :: Tile -> Id
getTileID (id, _, _, _, _) = id

getAnyBool :: [Bool] -> Bool
getAnyBool = foldl (||) False

countTrue :: [Bool] -> Int
countTrue = foldl (\acc x -> if x then acc + 1 else acc) 0

findCorners :: [Tile] -> [(Int, Int)]
findCorners tiles = sort res''
  where
    res = [ [ [ match tile' tile side | tile' <- tiles, tile' /= tile] | side <- [LeftS ..]] | tile <- tiles]
    res' = [ [getAnyBool (r !! i) | i <- [0..3] ] | r <- res ]
    res'' = zip  [ countTrue r | r <- res'] [0..]
    

main = do
   rs <- readFile "input.txt"
   let xs = lines rs
   let tiles = parse xs
   --print $ map getTileID $ findCorners tiles
   let firstTile = tiles !! 0
   let ids =  map snd $ take 4 $ findCorners tiles
   print [ getTileID (tiles !! id) | id <- ids]
   --print $ [[ match tile' firstTile side | tile' <- tiles, tile' /= firstTile ] | side <- [LeftS ..]]