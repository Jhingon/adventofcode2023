module Scratchcards (main) where

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Parser

data Card = Card Int [Int] [Int] deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 = sum . map (\(Card _ win have) -> score $ length $ win `intersect` have) . parse

part2 s = part2'
  where
    cards = parse s
    scoreMap = M.fromList $ [(n, score' card) | card@(Card n _ _) <- cards]
    part2' = sum $ map (scoreMap M.!) [1 .. length cards]
    score' (Card n wins have) =
      let win = length (wins `intersect` have)
          more = [n + 1 .. n + win]
       in sum (map score'' more) + 1
    score'' n = scoreMap M.! n

score 0 = 0
score n = 2 ^ (n - 1)

parse :: String -> [Card]
parse = map parse' . lines
  where
    parse' = fst . fromJust . runParser parseLine

ints = do
  x <- parseInt
  ( do
      xs <- many (char ' ' *> parseInt <|> parseString "  " *> parseInt)
      pure $ x : xs
    )
    <|> pure []

parseLine :: Parser Card
parseLine =
  Card
    <$> (parseString "Card" *> whitespace *> parseInt)
    <*> (char ':' *> whitespace *> ints)
    <*> (whitespace *> char '|' *> whitespace *> ints)
