module Expand () where

import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ solve 1 $ lines input
  print $ solve 999_999 $ lines input


shortestPath :: (Int,Int) -> (Int, Int) -> Int
shortestPath (x1,y1) (x2,y2) = abs (y2 - y1)+ abs (x2-x1)

findGalaxies :: [String] -> [(Int,Int)]
findGalaxies u = filter (\(i,j) -> u !! i !! j == '#') [(i,j) | i <- [0..(length u -1)], j <- [0..((length $ head u) - 1)]]

emptyRows :: Int -> [(Int, Int)] -> [Bool]
emptyRows n gs = map (\x -> x `notElem` (map fst gs)) [0..n] 

emptyCols :: Int -> [(Int,Int)] -> [Bool]
emptyCols n gs = map (\x -> x `notElem` (map snd gs)) [0..n]

expand :: Int -> [String] -> [(Int, Int)] -> [(Int,Int)]
expand f u gs = map (\(x,y) -> (x + emptyRowsBefore x, y + emptyColsBefore y)) gs
  where
    emptyRowsBefore x = (f*(length . filter id . take x $ emptyRows (length u) gs))
    emptyColsBefore y = (f*(length . filter id . take y $ emptyCols (length $ head u) gs))


solve f u = sum . map (uncurry shortestPath) . pairs . (expand f u) . findGalaxies $ u

pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
