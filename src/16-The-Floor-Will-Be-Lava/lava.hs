{-# LANGUAGE BangPatterns #-}

module Lava (main) where

import Control.Monad
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input.txt"
  print (part1 input)
  print (part2 input)

prettyprint :: [Point] -> IO ()
prettyprint ps = do
  mapM_ (\i -> print $ map (\j -> if (i, j) `elem` ps then '#' else '.') [0 .. 9]) [0 .. 9]

part1 = length . nub . join . parMap rpar allPointsBetween . f ((0, 0), East) . lines

part2 s = maximum $ map (\start -> let p = length . nub . join . map allPointsBetween $ f start ls) (startingPoints (length ls - 1, length (head ls) - 1))
  where
    ls = lines s

data Direction = North | South | East | West deriving (Eq, Ord, Show)

type Point = (Int, Int)

allPointsBetween :: (Point, Point) -> [Point]
allPointsBetween ((i1, j1), (i2, j2)) = [(i, j) | i <- irange, j <- jrange]
  where
    irange = [(min i1 i2) .. (max i1 i2)]
    jrange = [(min j1 j2) .. (max j1 j2)]

startingPoints :: (Int, Int) -> [(Point, Direction)]
startingPoints (maxI, maxJ) =
  [((maxI, bottom), North) | bottom <- [0 .. maxJ]]
    <> [((0, top), South) | top <- [0 .. maxJ]]
    <> [((left, 0), East) | left <- [0 .. maxI]]
    <> [((right, maxJ), West) | right <- [0 .. maxI]]

f :: (Point, Direction) -> [String] -> [(Point, Point)]
f (start, direction) ls = f' Set.empty (S.fromList (go start direction))
  where
    maxI = length ls - 1
    maxJ = length (head ls) - 1
    f' :: Set.Set (Point, Direction) -> S.Seq (Point, Direction) -> [(Point, Point)]
    f' _ S.Empty = []
    f' p ((current@(i, j), dir) S.:<| ds)
      | dir == North && i == 0 = f' p ds
      | dir == South && i == maxI = f' p ds
      | dir == East && j == maxJ = f' p ds
      | dir == West && j == 0 = f' p ds
      | Set.member (current, dir) p = f' p ds
      | otherwise =
          let p' = Set.insert (current, dir) p
              next = getNext current dir
              !ds' = ds S.>< S.fromList (go next dir)
           in pure (current, next) <> f' p' ds'
    go n@(i, j) d = case ls !! i !! j of
      '/' -> case d of
        North -> pure (n, East)
        South -> pure (n, West)
        West -> pure (n, South)
        East -> pure (n, North)
      '\\' -> case d of
        North -> pure (n, West)
        South -> pure (n, East)
        West -> pure (n, North)
        East -> pure (n, South)
      '|' -> case d of
        North -> pure (n, North)
        South -> pure (n, South)
        West -> pure (n, North) <> pure (n, South)
        East -> pure (n, South) <> pure (n, North)
      '-' -> case d of
        North -> pure (n, East) <> pure (n, West)
        South -> pure (n, West) <> pure (n, East)
        West -> pure (n, West)
        East -> pure (n, East)
      _otherwise -> [(n, d)]
    getNext = flip findEnd
    findEnd dir p@(i, j) =
      let n = findEndI dir p
       in case dir of
            North -> (n, j)
            South -> (n, j)
            East -> (i, n)
            West -> (i, n)
    findEndI dir (i, j) = case dir of
      North -> fromMaybe 0 (find (\x -> ls !! x !! j `elem` "/\\-") $ reverse [0 .. (max 0 $ i - 1)])
      South -> fromMaybe maxI (find (\x -> ls !! x !! j `elem` "/\\-") [(min maxI $ i + 1) .. maxI])
      West -> fromMaybe 0 (find (\x -> ls !! i !! x `elem` "/\\|") $ reverse [0 .. (max 0 $ j - 1)])
      East -> fromMaybe maxJ (find (\x -> ls !! i !! x `elem` "/\\|") [(min maxJ $ j + 1) .. maxJ])
