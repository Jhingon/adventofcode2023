{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Walk (main) where

import Control.Monad
import Control.Parallel.Strategies
import qualified Data.Array as A
import Data.Array.ST
import Data.Foldable
import Data.Ix
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Debug.Trace

type Maze = A.Array (Int, Int) Char

type Graph = A.Array (Int, Int) [(Point, Int)]

data Point = Point !Int !Int deriving (Eq, Show, Ord)

data D = D ((Int, Int), Int) deriving (Eq, Show)

instance Ord D where
  compare (D (_, d1)) (D (_, d2)) = compare d1 d2

data Direction = N | S | E | W deriving (Eq, Ord, Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
  let maze = A.listArray ((0, 0), (length ls - 1, length (head ls) - 1)) (join ls) :: Maze
  let ((lowerx, lowery), (upperx, uppery)) = A.bounds maze
  let (i, j) = fromJust $ L.find (\p -> maze A.! p == '.') [(lowerx, y) | y <- [lowery .. uppery]]
  let start = Point i j
  let prunedGraph = getPrunedGraph maze start

  let xs = (dfs fst (\(x, d) -> maybe [] (map (\(p, d') -> traceShowId (p, d + d'))) (M.lookup x prunedGraph)) [(start, 0)])
  -- mapM_ print xs
  -- print ""
  print . maximum . map snd $ [(Point a b, d) | (Point a b, d) <- xs, a == upperx]

rep' (D (p, d), direction) = p

opp = \case
  N -> S
  S -> N
  E -> W
  W -> E

next :: Maze -> Point -> [Point]
next maze (Point x y) =
  filter (\(Point a b) -> maze A.! (a, b) /= '#') (up <> down <> left <> right)
  where
    (_, (maxX, maxY)) = A.bounds maze
    up = if x > 0 then pure (Point (x - 1) y) else []
    down = if x < maxX then pure (Point (x + 1) y) else []
    left = if y > 0 then pure (Point x (y - 1)) else []
    right = if y < maxY then pure (Point x (y + 1)) else []

genGraph :: Maze -> Graph
genGraph maze = A.array (A.bounds maze) [(i, getPoints i) | i <- range (A.bounds maze)]
  where
    getPoints (x, y) = map (,1) $ next maze (Point x y)

dfs :: (Ord r) => (a -> r) -> (a -> [a]) -> [a] -> [a]
dfs rep nexts initial = toList $ go Set.empty (S.fromList initial)
  where
    go _ S.Empty = S.Empty
    go !seen (x S.:<| xs)
      | Set.member r seen = go seen xs
      | otherwise = withStrategy rpar (S.singleton x S.>< go seen' (S.fromList $ nexts x) S.>< go seen xs)
      where
        r = rep x
        seen' = Set.insert r seen

getPrunedGraph :: Maze -> Point -> M.Map Point [(Point, Int)]
getPrunedGraph maze start = M.fromList $ go Set.empty [(start, 0)]
  where
    go :: Set.Set Point -> [(Point, Int)] -> [(Point, [(Point, Int)])]
    go _ [] = []
    go !seen ((p, _) : ps)
      | Set.member p seen = go seen ps
      | otherwise = (p, new) : go seen' ps'
      where
        -- seen' = Set.union seen (Set.fromList hist)
        seen' = Set.insert p seen
        !new = f p
        !ps' = ps <> new
        f p' = map (getNexts 1 p') (next maze p)
          where
            getNexts :: Int -> Point -> Point -> (Point, Int)
            getNexts n prev curr =
              let nexts = filter (\x -> x /= prev) $ next maze curr
               in case length nexts of
                    0 -> (curr, n)
                    1 -> getNexts (n + 1) curr (head nexts)
                    _moreThanOne -> (curr, n)
        getPoints (Point x1 y1) (Point x2 y2) = map (uncurry Point) $ filter (\i -> maze A.! i /= '#') [i | i <- range ((x1, y1), (x2, y2)), i /= (x2, y2)]
