module  Main where
 
import qualified Data.Map as M
 
data Direction = East
               | West
               | SouthWest
               | SouthEast
               | NorthWest
               | NorthEast
            deriving (Show, Eq)
 
type Point = (Int, Int)
 
data Color = Black | White deriving (Show, Eq)
 
type Field = M.Map Point Color
 
move :: Point -> Direction -> Point
move (x,y) East = (x+2, y)
move (x,y) West = (x-2, y)
move (x,y) NorthEast = (x+1, y+1)
move (x,y) NorthWest = (x-1, y+1)
move (x,y) SouthEast = (x+1, y-1)
move (x,y) SouthWest = (x-1, y-1)
 
getDirections :: String -> [Direction]
getDirections "" = []
getDirections ('e':xs) = [East] ++ getDirections xs
getDirections ('w':xs) = [West] ++ getDirections xs
getDirections ('s':'e':xs) = [SouthEast] ++ getDirections xs
getDirections ('s':'w':xs) = [SouthWest] ++ getDirections xs
getDirections ('n':'e':xs) = [NorthEast] ++ getDirections xs
getDirections ('n':'w':xs) = [NorthWest] ++ getDirections xs
 
getCoord :: Point -> [Direction] -> Point
getCoord point [] = point
getCoord point (x:xs) = getCoord (move point x) xs
 
switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black
 
get' (Just x) = x
get' (Nothing) = Black
 
process :: Field -> [[Direction]] -> Field
process field [] = field
process field (x:xs) = process field' xs
  where
    coord = getCoord (0,0) x
    val = M.member coord field
    finalValue | val == False = (coord, Black)
               | otherwise = (coord , switchColor $ get' $ M.lookup coord field)
    field' = M.insert (fst finalValue) (snd finalValue) field
 
count :: Field -> Color -> Int
count field color = length $ filter ((==Black) . snd) list
  where
    list = M.toList field
 
main = do
  rs <- readFile "input.txt"
  let xs = lines rs
  let directions = map getDirections xs
  let field = process (M.fromList []) directions
  print $ count field White
  print $ getCoord (0,0) (getDirections "esew")
  print $ getCoord (0,0) (getDirections "nwwswee")
  print $ getCoord (0,0) (getDirections "se")
  print $ getCoord (0,0) (getDirections "nwwswee")
  print "ok"
