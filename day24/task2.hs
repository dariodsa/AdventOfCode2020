module  Main where
 
import qualified Data.Map as M
 
data Direction = East
               | West
               | SouthWest
               | SouthEast
               | NorthWest
               | NorthEast
            deriving (Show, Eq, Enum)
 
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
 
isWhite :: Field -> Point -> Bool
isWhite field point | M.member point field == False = True
                    | otherwise  = (get' $ M.lookup point field) == White
 
isBlack :: Field -> Point -> Bool
isBlack field point | M.member point field == True = (get' $ M.lookup point field) == Black
                    | otherwise = False
 
countNeigh :: Field -> Point -> Int
countNeigh field point = length $ filter (==True) neighs
  where
    neighs = [isBlack field (move point dirr) | dirr <- [East ..]]
 
count :: Field -> Color -> Int
count field color = length $ filter ((==Black) . snd) list
  where
    list = M.toList field
 
check :: Field -> Point -> Color -> (Point, Color)
check field point White | bcnt == 2 = (point, Black)
                        | otherwise = (point, White)
  where bcnt = countNeigh field point
check field point Black | bcnt == 0 || bcnt > 2 = (point, White)
                        | otherwise = (point, Black)
  where bcnt = countNeigh field point
 
run :: Int -> Field -> Field
run 0 field = field
run day field = run (day-1) field'
  where
    list = M.toList field
    list' =  [ check field point color | (point, color) <- list]
    field' = M.fromList list'
 
generate = [((x,y), White) | x <- [(-300),(-298)..300], y <-[(-100),(-98)..100] ] ++
           [((x,y), White) | x <- [(-301),(-299)..301], y <-[(-101),(-99)..101] ]
main = do
  rs <- readFile "input.txt"
  let xs = lines rs
  let directions = map getDirections xs
  let field = process (M.fromList (generate)) directions
  --let finalField = run 100 field
 
  --print $ count (run 1 field) Black
  --print $ [count (run i field) Black | i <- [1..100]]
 
  print $ count (run 100 field) Black
  print "ok"
