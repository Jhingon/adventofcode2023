module Spring () where

import Parser
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

main :: IO ()
main = do
  input <- readFile "input.txt"
  let m = part1 input
  let n = part2 input
  print m
  print (sum m)
  print n
  print (sum n)

part1 = map valid . parse 
part2 = map (valid . expand) . parse 

parse :: String -> [(String, [Int])]
parse = map (f . words) . lines
  where
    f [springs, ns] = (springs, readNs ns)
    readNs ns = fst $ fromJust $ (runParser (parseInt `sepBy` (satisfy (==','))) ns)

expand (s,i) = (init . join . take 5 . repeat $ (s <> pure '?'), join . take 5 . repeat $ i)

valid :: (String, [Int]) -> Int
valid (springs, dist) = valid'' springs dist
  where
    mem = M.fromList [((x,y), valid' (drop x springs) (drop y dist)) | x <- [0..length springs], y <- [0..length dist]]
    invalid s (x:xs)
      =  length s < x
      || '.' `elem` (take x s)
      || (length s > x && s !! x == '#')
    valid' [] [] = 1
    valid' ('#':_) [] = 0
    valid' [] _ = 0
    valid' s@('#':rest) xs@(y:ys)
      | invalid s xs = 0
      | otherwise = valid'' (drop (y+1) s) ys
    valid' s@('?':rest) xs = valid'' rest xs + valid' ('#':rest) xs
    valid' s@('.':rest) xs = valid' rest xs
    valid'' s xs = mem M.! (length springs - length s, length dist - length xs)

