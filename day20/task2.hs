{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (splitOn, pack, unpack)
import Data.List (sort)

type Id = Int
type Tile = (Id, String, String, String, String)

tileNE (id1, _ , _ , _ ,_) (id2, _ , _ , _ ,_) = id1 /= id2

type TileData = (Id, [String])

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
flipT (id, x1, x2, x3, x4) = (id, x3, reverse x2, x1, reverse x4)
  

apply :: Rotation -> Flip -> Tile -> Tile

apply DEG_0 NO_FLIP t = t
apply DEG_90 NO_FLIP t@(id, x1, x2, x3, x4) = (id, reverse x4, x1, reverse x2, x3)
apply DEG_180 NO_FLIP t = apply DEG_90 NO_FLIP $ apply DEG_90 NO_FLIP t
apply DEG_270 NO_FLIP t = apply DEG_90 NO_FLIP $ apply DEG_180 NO_FLIP t

apply DEG_0 FLIP t = flipT t
apply DEG_90 FLIP t@(id, x1, x2, x3, x4) = flipT (id, reverse x4, x1, reverse x2, x3)
apply DEG_180 FLIP t = flipT $ apply DEG_90 NO_FLIP $ apply DEG_90 NO_FLIP t
apply DEG_270 FLIP t = flipT $ apply DEG_90 NO_FLIP $ apply DEG_180 NO_FLIP t

flip' :: [String] -> [String]
flip' xs = [ xs !! i | let l = (length xs)-1, i <- [l,l-1..0]]

rotate' [] = []
rotate' xs = [ concat [ [(xs !! j) !! i] | j <- [l,l-1..0] ] | i <- [0..l]]
  where l = (length $ xs !! 0)-1

apply' :: Rotation -> Flip -> [String] -> [String]
apply' DEG_0 NO_FLIP xs = xs
apply' DEG_0 FLIP xs = flip' xs
apply' DEG_90 NO_FLIP xs = rotate' xs
apply' DEG_180 NO_FLIP xs = apply' DEG_90 NO_FLIP $ apply' DEG_90 NO_FLIP xs
apply' DEG_270 NO_FLIP xs = apply' DEG_90 NO_FLIP $ apply' DEG_180 NO_FLIP xs

apply' DEG_90 FLIP xs = flip' $ apply' DEG_90 NO_FLIP xs
apply' DEG_180 FLIP xs = flip' $ apply' DEG_180 NO_FLIP xs
apply' DEG_270 FLIP xs = flip' $ apply' DEG_270 NO_FLIP xs







parseData:: [String] -> [TileData]
parseData [] = []
parseData (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:_:xs) = [tile] ++ parseData xs
  where
    tile = (id, m)
    id = read $ unpack $ splitOn ":" (splitOn " " (pack x0) !! 1) !! 0
    m = [x1] ++ [x2] ++ [x3] ++ [x4] ++ [x5] ++ [x6] ++ [x7] ++ [x8] ++ [x9] ++ [x10]


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
    x4' = [head c | c <- m]


generate :: Tile -> [(Tile, Rotation, Flip)]
generate tile = [ (apply r f tile, r, f) | r <- [DEG_0 ..], f <- [FLIP ..] ]

match :: Tile -> Tile -> Side -> (Bool, Rotation, Flip)
match tile1 tile2 side = if length res == 0 then (False, DEG_0, NO_FLIP) else head res 
  where
    res = filter (\(r,_,_) -> r) [ (f' t1 tile2 side, r, f) | (t1, r, f) <- generate tile1]
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) LeftS = x2 == y4
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) RightS = x4 == y2
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) BottomS = x1 == y3
    f' (_, x1, x2, x3, x4) (_, y1, y2, y3, y4) UpS = x3 == y1

getMatchingTile :: Tile -> Side -> [Tile] -> (Tile, Rotation, Flip)
getMatchingTile tile side tiles = f ans
  where
    ans = head $ [ (tile', match tile' tile side) | tile' <- tiles, tileNE tile' tile, fstT $ match tile' tile side]
    f (t, (_, r, f)) = (t, r, f)


getTileID :: Tile -> Id
getTileID (id, _, _, _, _) = id

getAnyBool :: [Bool] -> Bool
getAnyBool = foldl (||) False

countTrue :: [Bool] -> Int
countTrue = foldl (\acc x -> if x then acc + 1 else acc) 0

fstT (x, _, _) = x

-- | get row by row
constructImage :: (Tile, Rotation, Flip) -> [Tile] -> Side -> [(Tile, Rotation, Flip)]
constructImage t tiles side = [t] ++ nextImg 0 t
  where
    nextImg 11 _ = []
    nextImg depth (tile, rot, flip) = [ans] ++ nextImg (depth + 1) ans
      where
        tileR = apply rot flip tile
        ans = getMatchingTile tileR side tiles
        
getIm :: (Tile, Rotation, Flip) -> [TileData] -> [String]
getIm ((id,_, _, _, _), r, f) tileData = res
  where res = apply' r f im
        im = snd $ head $ filter (\x -> id == fst x) tileData

getImage :: [[(Tile, Rotation, Flip)]] -> [TileData] -> [String]
getImage xs tiles = [ getRow x i | x <- xs, i <- [1..8]]
  where
    getRow s i = concat $ [ init $ tail ((getIm s' tiles) !! i) | s' <- s]

-- 012345678901234567# 
-- #    ##    ##    ###
--  #  #  #  #  #  #   


cm :: String -> String -> String -> Int
cm s1 s2 s3 = sum $ [ if f i then 1 else 0 | i <- [0..76]]
  where f i = ((s2 !! (i + 0) == '#') )
           && ((s3 !! (i + 1) == '#') )
           && ((s3 !! (i + 4) == '#') )
           && ((s3 !! (i + 7) == '#') )
           && ((s3 !! (i + 10) == '#') )
           && ((s3 !! (i + 13) == '#') )
           && ((s3 !! (i + 16) == '#') )
           && ((s2 !! (i + 5) == '#') )
           && ((s2 !! (i + 6) == '#') )
           && ((s2 !! (i + 11) == '#') )
           && ((s2 !! (i + 12) == '#') )
           && ((s2 !! (i + 17) == '#') )
           && ((s2 !! (i + 18) == '#') )
           && ((s2 !! (i + 19) == '#') )
           && ((s1 !! (i + 18) == '#') )

countMonster :: [String] -> Int
countMonster (x1:x2:x3:[]) = cm x1 x2 x3
countMonster (x1:x2:x3:xs) = (cm x1 x2 x3) + countMonster ([x2] ++ [x3] ++ xs)

main = do
   rs <- readFile "input.txt"
   let xs = lines rs
   let tiles = parse xs
   let tilesData = parseData xs
   --print $ map getTileID $ findCorners tiles
   let firstTile = tiles !! 0
   --print $ getMatchingTile firstTile RightS tiles
   --let first_row = constructImage (firstTile, DEG_0, NO_FLIP) tiles RightS
   let first_col = constructImage (firstTile, DEG_0, NO_FLIP) tiles BottomS
   --print first_col
   print firstTile
   print (fstT $ first_col !! 1)
   print $ apply DEG_270 FLIP (fstT $ first_col !! 1)
   print "Show"
   print $ match (fstT $ first_col !! 1) firstTile UpS
   let image = [constructImage start_tile tiles RightS | start_tile <- first_col]
   print image
   let finalImage = getImage image tilesData
   print $ map length finalImage
   let allImages = [ apply' r f finalImage | r <- [DEG_0 ..], f <- [FLIP ..]]
   print $ length $ (allImages !! 0)
   print $ map countMonster allImages
   --print finalImage
   print $ [ sum [length $ filter (=='#')  row |  row <- image ] | image <- allImages]
--   2828 too high