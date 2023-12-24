{-# LANGUAGE BangPatterns #-}

module Step (main) where

import Control.Monad
import Data.Containers.ListUtils
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

data Direction = U | D | L | R | None deriving (Eq, Ord, Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
  let start = findStart ls
  print input
  print (length $ fasterSearch 64 ls start)
  print (length $ fasterSearch 65 ls start)
  print (length $ fasterSearch (65 + 131) ls start)
  print (length $ fasterSearch (65 + 131 * 2) ls start)

-- assume it's possible to visit every position once in a non infinite map without
-- repeating positions
-- Number of possible positions= 7254
-- (26501365 `quot` 7254) * 7254 = 26498862
-- (26501365 `rem` 7254) = 2503
-- assume that whether you start from the center (where S is), or the center of any one of
-- the four sides (since there is a direct path from S to the center of each side) the number
-- of positions that can be visited in n steps is the same.
-- Number of positions that can be visited in 12955 steps = too many to bruteforce
-- Better way: length of 1 side of the (square) input = 131
-- then  (26501365 `quot` 131) = 202300
-- and   (26501365 `rem` 131) = 65
-- so
-- y1 = search 65
-- y2 = search (65+131)
-- y3 = search (65 + 131^2)
-- do a quadratic fit on y1 y2 y3 and plug in 202300

findNext :: [String] -> ((Int, Int), Int, Direction) -> [((Int, Int), Int, Direction)]
findNext ls ((i, j), n, _) = filter (\((x, y), _, _) -> ls !! x !! y /= '#') $ up <> down <> left <> right
  where
    maxI = length ls - 1
    maxJ = length (head ls) - 1
    up = if i > 0 then pure ((i - 1, j), n + 1, U) else []
    down = if i < maxI - 1 then pure ((i + 1, j), n + 1, D) else []
    left = if j > 0 then pure ((i, j - 1), n + 1, L) else []
    right = if j < maxJ - 1 then pure ((i, j + 1), n + 1, R) else []

findNextInf :: [String] -> (Int, Int) -> [(Int, Int)]
findNextInf ls (i, j) = filter (\(x, y) -> ls !! (x `mod` maxI) !! (y `mod` maxJ) /= '#') $ up <> down <> left <> right
  where
    maxI = length ls
    maxJ = length (head ls)
    up = pure (i - 1, j)
    down = pure (i + 1, j)
    left = pure (i, j - 1)
    right = pure (i, j + 1)

findStart :: [String] -> (Int, Int)
findStart ls = (x, y)
  where
    x = fromJust $ findIndex ('S' `elem`) ls
    y = fromJust $ elemIndex 'S' (ls !! x)

search :: (Ord r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
search rep next = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
      | S.member r seen = go seen xs
      | otherwise = pure x <> go seen' xs'
      where
        r = rep x
        !nexts = next x
        !xs' = xs <> nexts
        !seen' = S.insert r seen

fasterSearch :: Int -> [String] -> (Int, Int) -> [(Int, Int)]
fasterSearch n ls start = gon' n
  where
    gonMap = M.fromList [(i, gon i [start]) | i <- [1 .. n]]
    f :: [(Int, Int)] -> [(Int, Int)]
    f = nubOrd . join . map (findNextInf ls)
    gon :: Int -> [(Int, Int)] -> [(Int, Int)]
    gon 1 x = f x
    gon i _ = f $ gon' (i - 1)
    gon' i = gonMap M.! i
