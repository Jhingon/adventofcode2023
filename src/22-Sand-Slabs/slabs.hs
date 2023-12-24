{-# LANGUAGE BangPatterns #-}

module Slab (main) where

import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import Debug.Trace
import Parser

data Coord = C Int Int Int deriving (Eq, Show)

data Block = B Coord Coord deriving (Eq, Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
  let blocks = sortBy (\(B (C _ _ z1) _) (B (C _ _ z2) _) -> compare z1 z2) $ parse ls
  mapM_ print (reverse blocks)
  print ()
  let collapsed = fst $ collapse blocks
  -- let part1 = length . filter id $ parMap rpar (count collapsed) collapsed
  -- print part1
  let part2 = sum $ parMap rpar (count' collapsed) collapsed
  print part2

-- count :: [Block] -> Block -> Bool
-- count bs b = let pew = delete b bs == collapse (delete b bs) in traceShow (b, pew) pew

count' :: [Block] -> Block -> Int
count' bs b =traceShowId $ snd (collapse (delete b bs))
   

parse :: [String] -> [Block]
parse = map (fst . fromJust . runParser parseBlock)

parseBlock :: Parser Block
parseBlock = B <$> (parseCoord <* char '~') <*> parseCoord

parseCoord = C <$> (parseInt <* char ',') <*> (parseInt <* char ',') <*> parseInt

collapse :: [Block] -> ([Block], Int)
collapse = foldl f ([], 0)
  where
    f (!acc, !i) b =
      let newB =
            until
              (\(x@(B (C _ _ z1') _), _) -> any (`intersectB` x) acc || z1' == 1)
              (\(B (C l m n) (C o p q), j) -> (B (C l m (n - 1)) (C o p (q - 1)), j + 1))
              (b, 0)
       in (acc <> pure (fst newB), if snd newB > 0 then i + 1 else i)

intersectB :: Block -> Block -> Bool -- simplified since only multiples of 90 degrees are possible
intersectB (B (C x11 y11 z11) (C x12 y12 z12)) (B (C x21 y21 z21) (C x22 y22 z22)) =
  (z12 == (z21 - 1))
    && any
      (`elem` [(x', y') | x' <- [x21 .. x22], y' <- [y21 .. y22]])
      [(x, y) | x <- [x11 .. x12], y <- [y11 .. y12]]
