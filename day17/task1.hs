module Main where

import qualified Data.Map as M

data Cell = Active | Inactive
  deriving (Show, Eq)
type Coordinate = (Int, Int, Int)
type Field = M.Map Coordinate Cell

step = [-1, 0, 1]
cycles = 6

sumC :: Coordinate -> Coordinate -> Coordinate
sumC (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

readCell :: Char -> Cell
readCell '#' = Active
readCell  _  = Inactive

parse :: [String] -> Field
parse xs = M.fromList $ getActive' list
  where
    list = generate 0 xs
    generate _ [] = []
    generate n (x:xs) = first ++ generate (n+1) xs
      where first = map (\(x, y) -> ((n, x, 0), readCell y )) $ zip [0..] x
    getActive' = filter $ (==Active) . snd

isActive :: Field -> Coordinate -> Bool
isActive filed x = f value
  where
    value = M.lookup x filed
    f (Nothing) = False
    f (Just Active ) = True
    f _ = False

getNeighbours :: Field -> Coordinate -> [Coordinate]
getNeighbours field coo = [sumC coo (x, y, z) | x <- step, y <- step, z <- step, (x, y, z) /= (0,0,0)]

getNumOfActive :: Field -> Coordinate -> Int
getNumOfActive field coo = length neigh_active
  where neigh_coo = getNeighbours field coo
        neigh_active = filter (isActive field) neigh_coo

getActive :: Field -> [Coordinate]
getActive = M.foldlWithKey (\acc key val -> if val == Active then [key] ++ acc else acc) []

nextStage :: Cell -> Field -> Coordinate -> Cell
nextStage Active field coo   = if val == 3 || val == 2  then Active else Inactive
  where
    val = getNumOfActive field coo
nextStage Inactive field coo = if val == 3 then Active else Inactive
  where
    val = getNumOfActive field coo

getState :: Field -> Coordinate -> Cell
getState field coo = f val
  where
    val = M.lookup coo field
    f Nothing = Inactive
    f (Just x) = x

nextCycle :: Field -> Int -> Field
nextCycle field 0 = field
nextCycle field n = (nextCycle field' (n-1))
  where field' = M.fromList xs
        active = getActive field
        candidates = active ++ (concat $ map (getNeighbours field) active)
        xs = map (\coo -> (coo, nextStage (getState field coo) field coo)) candidates
        

getFinalAns :: Field -> Int
getFinalAns = foldl (\acc y -> if y == Active then acc + 1 else acc) 0

main = do 
  rs <- readFile "input.txt"
  let field = parse $ lines rs
  print $ getActive field
  print $ getNeighbours field (0,0,0)
  print $ getNumOfActive field (0,0,0)
  print $ getFinalAns $ nextCycle field cycles
  
