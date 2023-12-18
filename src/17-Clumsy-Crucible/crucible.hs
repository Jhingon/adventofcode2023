{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Crucible (main) where

import Control.Monad
import qualified Data.Array.Unboxed as A
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
  let edges = A.listArray ((0, 0), (length ls - 1, length (head ls) - 1)) . join $ map readL ls :: A.UArray (Int, Int) Int
  let (low, high) = A.bounds edges
  let pew1 = dijkstra id (nextDirection edges 0 3) [Lava low 0 E, Lava low 0 S]
  let pew2 = dijkstra id (nextDirection edges 4 10) [Lava low 0 E, Lava low 0 S]
  print $ L.sort [(x, y) | (Lava x _ _, y) <- pew1, x == high]
  print $ L.sort [(x, y) | (Lava x s _, y) <- pew2, x == high, s > 4]

readL :: [Char] -> [Int]
readL = map (read . pure)

nextDirection :: A.UArray (Int, Int) Int -> Int -> Int -> Lava -> [DijkstraState Lava]
nextDirection edges low high (Lava loc n d)
  | d == None = genStates [incP edges 0 loc x | x <- [N, S, W, E]]
  | n >= low && n < high = genStates [left edges d loc, right edges d loc, straight edges d loc]
  | n < low = genStates [straight edges d loc]
  | otherwise = genStates [left edges d loc, right edges d loc]
  where
    genStates = mapMaybe (fmap genDS)
    genDS (point, n', dir) = DS (Lava point n'' dir) (edges A.! point)
      where
        n'' = if n' == 0 then 1 else n' + n

incP :: Edges -> Int -> (Int, Int) -> Direction -> Maybe ((Int, Int), Int, Direction)
incP edges k (i, j) = \case
  N -> if i == 0 then Nothing else Just ((i - 1, j), k, N)
  S -> if i == maxI then Nothing else Just ((i + 1, j), k, S)
  E -> if j == maxJ then Nothing else Just ((i, j + 1), k, E)
  W -> if j == 0 then Nothing else Just ((i, j - 1), k, W)
  _ -> error "Shouldn't Happen"
  where
    (_, (maxI, maxJ)) = A.bounds edges

left :: Edges -> Direction -> (Int, Int) -> Maybe ((Int, Int), Int, Direction)
left edges d point = case d of
  N -> incP edges 0 point W
  S -> incP edges 0 point E
  E -> incP edges 0 point N
  W -> incP edges 0 point S
  _ -> error "Shouldn't happen"

straight :: Edges -> Direction -> (Int, Int) -> Maybe ((Int, Int), Int, Direction)
straight edges d point = case d of
  N -> incP edges 1 point N
  S -> incP edges 1 point S
  E -> incP edges 1 point E
  W -> incP edges 1 point W
  _ -> error "Shouldn't happen"

right :: Edges -> Direction -> (Int, Int) -> Maybe ((Int, Int), Int, Direction)
right edges d point = case d of
  N -> incP edges 0 point E
  S -> incP edges 0 point W
  E -> incP edges 0 point S
  W -> incP edges 0 point N
  _ -> error "Shouldn't happen"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : as) = Just a

type Edges = A.UArray (Int, Int) Int

data Lava = Lava {location :: (Int, Int), streak :: Int, prevD :: Direction} deriving (Eq, Ord, Show)

data Direction = N | S | E | W | None deriving (Eq, Ord, Show)

data A = A Int (Int, Int) [Direction] deriving (Eq, Show)

instance Ord A where
  compare (A n1 _ _) (A n2 _ _) = compare n1 n2

class PriorityQueue q where
  extractMin :: (Ord a) => q a -> Maybe (a, q a)
  (+++) :: (Ord a) => q a -> q a -> q a
  singleton :: (Ord a) => a -> q a
  insert :: (Ord a) => a -> q a -> q a
  insert a q = singleton a +++ q
  fromList :: (Ord a) => [a] -> q a
  toList :: (Ord a) => q a -> [a]

data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Eq, Show)

instance PriorityQueue SkewHeap where
  extractMin Empty = Nothing
  extractMin (SkewNode a l r) = Just (a, l +++ r)
  Empty +++ q = q
  q +++ Empty = q
  q1@(SkewNode a1 l1 r1) +++ q2@(SkewNode a2 l2 r2)
    | a1 <= a2 = SkewNode a1 (q2 +++ r1) l1
    | otherwise = SkewNode a2 (q1 +++ r2) l2
  singleton a = SkewNode a Empty Empty
  toList Empty = []
  toList q = maybe [] (\(a, q') -> a : toList q') (extractMin q)
  fromList [] = Empty
  fromList (x : xs) = insert x $ fromList xs

data DijkstraState a = DS {node :: a, distance :: Int} deriving (Eq, Show)

-- not exactly dijkstra since we don't maintain a distance array but whatever
dijkstra :: (Ord a, Ord r) => (a -> r) -> (a -> [DijkstraState a]) -> [a] -> [(a, Int)]
dijkstra convert next initial = go Set.empty initialQ
  where
    initialQ = fromList [(0, i) | i <- initial]
    go _ Empty = []
    go prev statequeue
      | Set.member track prev = go prev rest
      | otherwise = (currentstate, dist) : go prev' rest'
      where
        ((dist, currentstate), rest) = fromJust $ extractMin statequeue
        track = convert currentstate
        prev' = Set.insert track prev
        rest' = L.foldl' f rest (next currentstate)
        f :: (Ord a) => SkewHeap (Int, a) -> DijkstraState a -> SkewHeap (Int, a)
        f q newstate = insert (dist + newstate.distance, newstate.node) q
