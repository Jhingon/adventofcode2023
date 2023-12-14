module Parabolic (main) where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ls = lines input
  print ""
  print $ score $ part2 ls

part1 :: [String] -> Int
part1 ls = score $ foldl dropLine ls [0 .. (length ls - 1)]

part2 :: [String] -> [String]
part2 = getFinal 1000000000 []
  where
    part2' l = foldl (\l' d -> foldl (\l'' i -> dropLine' l'' d i) l' [0 .. (length l - 1)]) l ([N, W, S, E])
    getFinal :: Int -> [[String]] -> [String] -> [String]
    getFinal target prev l =
      let l' = part2' l
       in case l' `elemIndex` prev of
            Nothing -> getFinal target (prev <> pure l') l'
            Just n ->
              let cyc = length prev - n
                  cyclesTillTarget = (target - n) `mod` cyc
                  lastcycle = drop (cyclesTillTarget - 1) $ drop n prev
               in head lastcycle

dropLine' :: [String] -> Direction -> Int -> [String]
dropLine' ls N i = foldl (\ls' j -> if ls' !! j !! i == 'O' then dropAllTheWay' ls' N (j, i) else ls') ls [0 .. (length (head ls) - 1)]
dropLine' ls W i = foldl (\ls' j -> if ls' !! j !! i == 'O' then dropAllTheWay' ls' W (j, i) else ls') ls [0 .. (length (head ls) - 1)]
dropLine' ls S i = reverse $ foldl (\ls' j -> if ls' !! j !! i == 'O' then dropAllTheWay' ls' N (j, i) else ls') (reverse ls) [0 .. (length (head ls) - 1)]
dropLine' ls E i = transpose . reverse . transpose $ foldl (\ls' j -> if ls' !! j !! i == 'O' then dropAllTheWay' ls' W (j, i) else ls') (transpose . reverse . transpose $ ls) [0 .. (length (head ls) - 1)]

dropLine :: [String] -> Int -> [String]
dropLine ls i = foldl (\ls' j -> if ls' !! j !! i == 'O' then dropAllTheWay ls' (j, i) else ls') ls [0 .. (length (head ls) - 1)]

data Direction = N | S | E | W

dropAllTheWay' :: [String] -> Direction -> (Int, Int) -> [String]
dropAllTheWay' ls d (j, i) = replace2 (replace2 ls '.' (j, i)) 'O' (newPositionX, newPositionY)
  where
    newPositionX = case d of
      N -> goNorth j
      S -> goSouth j
      W -> j
      E -> j
    newPositionY = case d of
      N -> i
      S -> i
      W -> goWest i
      E -> goEast i
    goNorth x
      | x == 0 = x
      | ls !! (x - 1) !! i == '.' = goNorth (x - 1)
      | ls !! (x - 1) !! i == '#' = x
      | ls !! (x - 1) !! i == 'O' = x
    goSouth x
      | x == length ls - 1 = x
      | ls !! (x + 1) !! i == '.' = goSouth (x + 1)
      | ls !! (x + 1) !! i == '#' = x
      | ls !! (x + 1) !! i == 'O' = x
    goWest x
      | x == 0 = x
      | ls !! j !! (x - 1) == '.' = goWest (x - 1)
      | ls !! j !! (x - 1) == '#' = x
      | ls !! j !! (x - 1) == 'O' = x
    goEast x
      | x == (length $ head ls) - 1 = x
      | ls !! j !! (x + 1) == '.' = goEast (x + 1)
      | ls !! j !! (x + 1) == '#' = x
      | ls !! j !! (x + 1) == 'O' = x

dropAllTheWay :: [String] -> (Int, Int) -> [String]
dropAllTheWay ls (j, i) = replace2 (replace2 ls '.' (j, i)) 'O' (newPosition, i)
  where
    newPosition = goDown j
    goDown x
      | x == 0 = x
      | ls !! (x - 1) !! i == '.' = goDown (x - 1)
      | ls !! (x - 1) !! i == '#' = x
      | ls !! (x - 1) !! i == 'O' = x

replace :: [a] -> a -> Int -> [a]
replace xs a i = take i xs <> pure a <> drop (i + 1) xs

replace2 :: [[a]] -> a -> (Int, Int) -> [[a]]
replace2 xss a (i, j) = replace xss (replace (xss !! i) a j) i

score :: [String] -> Int
score ls = foldl (\acc x -> acc + x * countOs ls (length ls - x)) 0 $ reverse [1 .. length ls]

countOs :: [String] -> Int -> Int
countOs ls i = length $ filter (== 'O') $ ls !! i
