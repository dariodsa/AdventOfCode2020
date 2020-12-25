module Main where

getVal :: Integer -> Integer -> Integer -> [(Integer, Integer)]
getVal subjectN prev loop = [(newV, loop)] ++ getVal subjectN newV (loop + 1)
  where newV = (prev * subjectN) `mod` 20201227

main :: IO()
main = do 
  let doorPubK = 14222596
  let cardPubK = 4057428
  let subjectN = 7
  let doorLoopSize = snd $ head $ filter ((== doorPubK) . fst) $ getVal subjectN 1 1
  let cardLoopSize = snd $ head $ filter ((== cardPubK) . fst) $ getVal subjectN 1 1
  print $ last $ take (fromIntegral doorLoopSize) $ getVal cardPubK 1 1
