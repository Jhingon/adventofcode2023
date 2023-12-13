module Maze () where

import Data.Maybe
import Data.List
import Control.Monad

main :: IO ()
main = do
  input <- readFile "input.txt"
  let graph' = lines input
  let (_,_,n) = head $ part1 graph'
  let (Point sx sy) = findStart graph'
  let graph = replaceL2 '|' (sy, sx) graph'
  let visited = [(sy, sx)] <> (map (\(Point x y,_,_) -> (y,x)) $ join $ take n (f' graph' (Point sx sy)))
  let inside = foldl (\acc x -> if withinLoop graph x visited then acc <> pure x else acc) [] [(i,j) | i <- [0..((length graph)-1)], j <- [0..((length (head graph))-1)]]
  let ng = foldl (\acc x -> replaceL2 'I' x acc) graph inside
  print $ length inside
  

withinLoop :: [String] -> (Int, Int) -> [(Int,Int)] -> Bool
withinLoop loops (i,j) visited = (not $ (i,j) `elem` visited) && (length $ filter (\x -> x == '|' || x == 'F' || x == '7') right) `mod` 2 /= 0
  where
    xlength = length $ head loops
    ylength = length loops
    right = map (\(i',j') -> loops !! i' !! j') $ filter (\(y,x) -> y == i && x >= j) visited
    left = map (\(i',j') -> loops !! i' !! j') $ filter (\(y,x) -> y == i && x <= j) visited

replaceL :: a -> Int -> [a]-> [a]
replaceL x i xs= (take i xs) <> pure x <> drop (i+1) xs

replaceL2 :: a -> (Int, Int) -> [[a]] -> [[a]]
replaceL2 x (i,j) xxs = replaceL (replaceL x j (xxs !! i)) i xxs

part1 :: [String] -> [(Point,Move,Int)]
part1 graph = f graph (findStart graph) 

findStart :: [String] -> Point
findStart graph = findStart' 0
  where
    findStart' n = if isJust $ i n then Point (fromJust $ i n) n else findStart' (n+1)
    i n = findIndex (=='S') (graph !! n)
 
data Direction = North | South | East | West deriving (Eq, Show)
data Point = Point Int Int deriving (Eq, Show)
data Move = Move Point Direction deriving (Eq, Show)

f' :: [String] -> Point -> [[(Point, Move, Int)]]
f' graph start = iterate (map (\(point,m,i) -> move graph point m i)) (starts)
  where
    allSame :: [(Point, Move, Int)] -> Bool
    allSame [] = True
    allSame ((p, _,_):ps)= all (\(p', _, _) -> p == p') ps
    starts :: [(Point,Move,Int)]
    starts = map (\(point,m,i) -> (move graph point m i)) $ filter (\(point, (Move _ dir), _) -> isValid graph point dir) $ (zip3 (repeat start) (map (\x -> Move (decodeDirection x) x) [North,South,East,West]) (repeat 0))

f :: [String] -> Point -> [(Point, Move, Int)]
f graph start = until (allSame) (map (\(point,m,i) -> move graph point m i)) (starts)
  where
    allSame :: [(Point, Move, Int)] -> Bool
    allSame [] = True
    allSame ((p, _,_):ps)= all (\(p', _, _) -> p == p') ps
    starts :: [(Point,Move,Int)]
    starts = map (\(point,m,i) -> (move graph point m i)) $ filter (\(point, (Move _ dir), _) -> isValid graph point dir) $ (zip3 (repeat start) (map (\x -> Move (decodeDirection x) x) [North,South,East,West]) (repeat 0))

sumP :: Point -> Point -> Point
sumP (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

move :: [String] -> Point -> Move -> Int -> (Point, Move, Int)
move graph start@(Point x1 y1) m@(Move p@(Point x' y') d) i = (nextP, nextD, i+1)
  where
    nextP = sumP start p
    nextD = Move (decodeDirection nd) nd
    nd = case get graph nextP of
      '|' -> case d of
          North -> North
          South -> South
      '-' -> case d of
          East  -> East
          West  -> West
      'L' -> case d of
          South -> East
          West  -> North
      'J' -> case d of
          South -> West
          East  -> North
      '7' -> case d of
          East  -> South
          North -> West
      'F' -> case d of
          West  -> South
          North -> East
      _   -> error $ (show start) <> " " <> (show m) <> show (i)

decodeDirection :: Direction -> Point
decodeDirection North = Point  0 (-1)
decodeDirection South = Point 0 (1)
decodeDirection West = Point (-1) 0
decodeDirection East = Point 1 0

isValid :: [String] -> Point -> Direction -> Bool
isValid graph p d = case get graph $ sumP p (decodeDirection d) of
  '.' -> False
  '|' -> d == North || d == South
  '-' -> d == East  || d == West
  'L' -> d == South || d == West
  'J' -> d == South || d == East
  '7' -> d == North || d == East
  'F' -> d == North || d == West
  'e' -> False
  _   -> False

get :: [String] -> Point -> Char
get graph (Point x y) = if x < 0 || y < 0 || x >= length (graph !! 0) || y >= length graph then 'e' else graph !! y !! x
