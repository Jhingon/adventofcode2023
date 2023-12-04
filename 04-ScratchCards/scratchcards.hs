module Scratchcards () where

import Parser
import Data.Char (isDigit, isSpace)
import Data.List (lookup, foldl')
import Control.Applicative

parseNum :: Parser Int
parseNum = read <$> some (satisfy isDigit)

whitespace :: Parser String
whitespace = many (satisfy isSpace)

parseString :: String -> Parser String
parseString [x] = liftA2 (:) (satisfy (==x)) (pure [])
parseString (x:xs) = liftA2 (:) (satisfy (==x)) (parseString xs)

parseNumList :: Parser [Int]
parseNumList = liftA2 (:) parseNum (many rest)
  where
    rest = whitespace *> parseNum

parseLine :: Parser ([Int], [Int])
parseLine = (\xs ys -> (xs, ys)) <$>
  ((parseString "Card") *> whitespace *> parseNum *> satisfy(==':') *> whitespace *>
  parseNumList <* whitespace) <*> (satisfy (=='|') *> whitespace *> parseNumList)

countMatches :: ([Int], [Int]) -> Int
countMatches (card, winning) = length $ filter (`elem` winning) card

score :: ([Int], [Int]) -> Int
score (card, winning) = f (length $ filter (`elem` winning) card)
  where
    f 0 = 0
    f 1 = 1
    f n = 2^(n - 1)

lookup' :: Int -> [(Int, Int)] -> Int
lookup' i xs = case lookup i xs of
  Nothing -> 0
  Just x -> x

countCard :: [(Int, Int)] -> Int -> Int
countCard cards i = 1 + (foldl' (\acc x -> acc + countCard' cards x) 0 [(i+1) .. (i+value)])
  where
    value = lookup' i cards
    countCard' cards' i' = countCard cards' i'

part1 :: String -> Int
part1 input = sum scores 
  where
    scores = map (score) $ map (parse) (lines input)

parse s = case runParser parseLine s of
  Just (n, _) -> n
  Nothing -> error s

solve :: IO ()
solve = do
  ex <- readFile "04-ScratchCards/example.txt" 
  input <- readFile "04-ScratchCards/input.txt"
  print $ part1 ex
  print $ part1 input
  let c = zip [1..] $ map countMatches $ map parse (lines ex)
  let c'= zip [1..] $ map countMatches $ map parse (lines input)
  let cardmap = map (countCard c) [1..length c]
  let cardmap' = map (countCard c') [1..length c']
  print (sum cardmap)
  print (zip [1..] cardmap')
  print (sum cardmap')
  -- print (foldl' (\acc x -> acc + countCard c' x) 0 [1..length c'])
