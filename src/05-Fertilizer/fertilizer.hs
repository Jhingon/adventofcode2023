module Fertilizer (main) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import Parser

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 s = minimum $ map (\seed -> foldl applyTransform seed rs) seeds
  where
    (Seeds seeds, fs) = parse s
    rs = map convertToRanges fs

part2 s = sort $ map (\(Range a b) -> (a, b)) $ foldl (\seeds' (R r) -> applyTransform' seeds' r) seeds rs
  where
    (ss, fs) = parse s
    seeds = seedToRanges ss
    rs = map convertToRanges fs

data Seeds = Seeds [Int] deriving (Show)

data Func = F [(Int, Int, Int)] deriving (Show)

data Range = Range Int Int deriving (Show)

data R = R [(Range, Range)] deriving (Show)

parseSeeds :: Parser Seeds
parseSeeds = Seeds <$> (parseString "seeds:" *> whitespace *> (parseInt `sepBy` char ' '))

int = whitespace *> parseInt

parseMap :: Parser Func
parseMap = F <$> (whitespace *> (many (satisfy (/= ':')) *> char ':' *> whitespace *> many ((,,) <$> int <*> int <*> int)))

parse :: String -> (Seeds, [Func])
parse = fst . fromJust . runParser ((,) <$> parseSeeds <*> many parseMap)

convertToRanges :: Func -> R
convertToRanges (F xs) = R $ map (\(to, from, l) -> (Range to $ to + l, Range from $ from + l)) xs

seedToRanges :: Seeds -> [Range]
seedToRanges (Seeds xs) = convert' xs
  where
    convert' [] = []
    convert' [_] = error "ugh"
    convert' (x1 : x2 : xs) = [Range x1 $ x1 + x2] <> convert' xs

withinRange :: Int -> Range -> Bool
withinRange n (Range s e) = n >= s && n < e

applyTransform :: Int -> R -> Int
applyTransform i (R xs) = go xs
  where
    go [] = i
    go ((Range x1 _, from@(Range y1 _)) : ys)
      | withinRange i from = i - y1 + x1
      | otherwise = go ys

applyTransform' :: [Range] -> [(Range, Range)] -> [Range]
applyTransform' seeds rs = foldl (\acc s -> acc <> go s rs) [] seeds
  where
    go seed [] = pure seed
    go (Range s1 s2) ((Range t1 _, Range r1 r2) : xs)
      | s1 < r1 && s2 > r2 = go (Range s1 (r1 )) xs <> go (Range r2 s2) xs <> [f r1 r2 r1 t1]
      | s1 < r1 && s2 < r2 && s2 > r1 = go (Range s1 (r1 )) xs <> [f r1 s2 r1 t1]
      | s1 >= r1 && s2 < r2 && s2 > r1 = [f s1 s2 r1 t1]
      | s1 < r2 && s2 > r2 && s1 >= r1 = go (Range r2 s2) xs <> [f s1 r2 r1 t1]
      | otherwise = go (Range s1 s2) xs
    f a b i t = Range (t + a - i) (t + b - i)
