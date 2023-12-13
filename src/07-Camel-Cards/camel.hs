module Camel () where

import Parser
import Data.List (sort, sortBy,  group)

solve :: IO ()
solve = do
  input <- readFile "07-Camel-Cards/input.txt"
  print $ part1 input

data Hand = Hand [Int] deriving (Show, Eq)

data HandType = Five | Four | FullHouse | Three | TwoPair | Pair | HighCard deriving (Eq, Ord, Show)

part1 = sum . map (uncurry (*)) . zip [1..] . map snd . sortBy (\(h1, _) (h2, _) -> compare h2 h1) . parse

readHand :: Hand -> HandType
readHand (Hand xs) = case groupings of
  1 -> Five
  2 -> if four groups then Four else FullHouse
  3 -> if three groups then Three else TwoPair
  4 -> Pair
  5 -> HighCard
  where
    groupings = length groups
    (Hand ys) = convertJokerHand (Hand xs)
    groups = group $ sort ys
    four [x1, x2] = length x1 == 4 || length x2 == 4
    three [x1,x2,x3] = length x1 == 3 || length x2 == 3 || length x3 == 3
    
instance Ord Hand where
  compare h1@(Hand xs) h2@(Hand ys) 
    | readHand h1 < readHand h2 = LT
    | readHand h1 > readHand h2 = GT
    | readHand h1 == readHand h2 = f xs ys

f :: [Int] -> [Int] -> Ordering
f [] [] = EQ
f (x:xs) (y:ys) = if x == y then f xs ys else compare y x

parse :: String -> [(Hand, Int)]
parse s = map (\x -> (Hand $ map convert $ (words x) !! 0, read $ (words x) !! 1)) (lines s)
  where
    convert 'A' = 14
    convert 'K' = 13
    convert 'Q' = 12
    convert 'J' = 1
    convert 'T' = 10
    convert x = read [x] 

convertJokerHand :: Hand -> Hand
convertJokerHand (Hand [1,1,1,1,1]) = Hand [14,14,14,14,14]
convertJokerHand (Hand xs) = if ys !! 0 !! 0 == 1 then newHand $ longestGroupType (tail ys) else Hand xs
  where
    ys = group (sort xs)
    longestGroupType [] = 0
    longestGroupType ms = maximum $ map (!! 0) $ filter (\x -> length x == maxlength ms) ms
    maxlength = fst . maximum . map (\x -> (length x, x))
    newHand n = Hand $ map (\x -> if x == 1 then n else x) xs
