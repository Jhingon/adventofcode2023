{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lagoon (main) where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe
import Debug.Trace
import Parser

main :: IO ()
main = do
  input <- readFile "input.txt"
  print (part1 . parse $ input)
  print (part2 . parse' $ input)

data Direction = U | D | L | R deriving (Eq, Show)

data Instruction = I Direction Int deriving (Eq, Show)

part1 is =
  (+ (1 + foldl (\acc (I _ n) -> n + acc) 0 is `quot` 2))
    . shoelace
    . getPairs
    . getVertices
    $ is

part2 is =
  (+ (1 + foldl (\acc (I _ n) -> n + acc) 0 is `quot` 2))
    . shoelace
    . getPairs
    . getVertices
    . (\x -> traceShow x x)
    $ is

shoelace :: [((Int, Int), (Int, Int))] -> Int
shoelace v = abs (foldl f 0 v `quot` 2)
  where
    f acc ((x1, y1), (x2, y2)) = (y1 + y2) * (x1 - x2) + acc

parse :: String -> [Instruction]
parse = map (fst . fromJust . runParser parseInstruction) . lines

parse' :: String -> [Instruction]
parse' = map (fst . fromJust . runParser parseNewInstruction) . lines

parseNewInstruction :: Parser Instruction
parseNewInstruction =
  (\(i, d) -> I d i)
    <$> (pd *> whitespace *> parseInt *> whitespace *> char '(' *> parseHex <* char ')')

parseHex :: Parser (Int, Direction)
parseHex = convert <$> (char '#' *> some (satisfy (`elem` "0123456789abcdef")))
  where
    convert hex = let (!l, !hex') = r $ reverse hex in (foldl f 0 hex', c l)
    c = \case
      '0' -> R
      '1' -> D
      '2' -> L
      '3' -> U
    r :: String -> (Char, String)
    r [] = ('e', [])
    r (x : xs) = (x, reverse xs)
    f :: Int -> Char -> Int
    f acc n = 16 * acc + fromMaybe 0 (n `elemIndex` "0123456789abcdef")

parseInstruction :: Parser Instruction
parseInstruction =
  I
    <$> (pd <* whitespace)
    <*> parseInt
    <* many (satisfy (const True))

pd :: Parser Direction
pd =
  (char 'U' $> U)
    <|> (char 'D' $> D)
    <|> (char 'L' $> L)
    <|> (char 'R' $> R)

getVertices :: [Instruction] -> [(Int, Int)]
getVertices = foldl f []
  where
    f prev (I d n) =
      let recent = fromMaybe (0, 0) (getLast prev)
       in prev <> [goDirection d n recent]

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (x1 : x2 : xs) = (x1, x2) : getPairs (x2 : xs)

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast xs = Just (last xs)

goDirection :: Direction -> Int -> (Int, Int) -> (Int, Int)
goDirection d n (i, j) = case d of
  U -> (i - n, j)
  D -> (i + n, j)
  L -> (i, j - n)
  R -> (i, j + n)
