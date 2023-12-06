module Fertilizer () where

import Parser
import Data.List (find, foldl',sort)
import Control.Monad (join)
import Control.Applicative
import Data.Char (isSpace, isLetter)
import Control.Parallel.Strategies

main = solve

solve :: IO ()
solve = do
  ex <- readFile "05-Fertilizer/ex.txt"
  input <- readFile "05-Fertilizer/input.txt"
  print (part2 ex)
  print (part2 input)

data M = M Int Int Int deriving (Show)

r :: Int -> Int -> (Int, Int)
r a b = (a, a + b)

intSpace :: Parser Int
intSpace = parseInt <* satisfy (==' ')

parseSeed :: Parser (Int, Int)
parseSeed = r <$> (whitespace *> intSpace) <*> parseInt

parseSeeds :: Parser [(Int,Int)]
parseSeeds = parseString "seeds: " *> (many parseSeed) <* whitespace

parseMaps :: Parser [M]
parseMaps = title *> many (M <$> intSpace <*> intSpace <*> parseInt <* satisfy(=='\n'))
  where
    title = whitespace *> (many $ satisfy (\x -> isLetter x || x == '-')) *> parseString " map:\n" 

parseAllMaps :: Parser [[M]]
parseAllMaps = many parseMaps

parse :: Parser a -> String -> (a, String)
parse p i = case runParser p i of
  Nothing -> error "Oh No"
  Just x -> x

f :: (Int, Int) -> [M] -> [(Int, Int)]
f s [] = [s]
f (s1, s2) (M d s l : ms)
  | overlap = before <> mapped <> after
  | otherwise = f (s1,s2) ms
  where
    (m1,m2) = r s l
    (d1,d2) = r d l
    before = if s1 < m1 then f (s1, m1) ms else []
    after = if s2 > m2 then f (m2, s2) ms else []
    mapped = [(dstart, dend)]
    dstart = d1 + max 0 (s1 - m1)
    dend = if s2 < m2 then dstart + (s2 - s1) else d2
    overlap = (s2 > m1 && s2 < m2) || (s1 < m2 && s1 > m1) || (m1 > s1 && m2 < s2)

-- for some reason the second lowest number is the correct answer
part2 input = take 5 $ sort $ map fst $ foldl' (\seeds' m -> join $ map (\seed -> f seed m) seeds') seeds maps
  where
    (seeds, rest) = parse parseSeeds input
    (maps, _) = parse parseAllMaps rest
  
