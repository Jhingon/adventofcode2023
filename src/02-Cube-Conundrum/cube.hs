module Cube () where

import Parser
import Data.Char (isDigit, isSpace)
import Control.Applicative
import Data.List (any, find)

data Round = Round { red :: Int, green :: Int, blue :: Int } deriving (Show)

data Game = Game { no :: Int, rounds :: [Round] } deriving (Show)

parseGame :: Parser Game
parseGame
  = Game 
  <$> (parseString "Game" *> whitespace *> parseNum <* satisfy (==':') <* whitespace)
  <*> parseRounds

parseRounds :: Parser [Round]
parseRounds = liftA2 (:) parseRound (many rest) 
  where
    rest = satisfy (==';') *> whitespace *> parseRound

parseRound :: Parser Round
parseRound = do
  cubes <- parseCubes
  pure (Round (getCount "red" cubes) (getCount "green" cubes) (getCount "blue" cubes))
  where
    getCount color cubes = case find (\(_, col) -> color == col) cubes of
      Nothing -> 0
      Just (count, _) -> count

parseCube :: Parser (Int, String)
parseCube = (,) <$> (parseNum <* whitespace) <*> parseColor

parseCubes :: Parser [(Int, String)]
parseCubes = liftA2 (:) parseCube (many rest)
  where
    rest = satisfy (==',') *> whitespace *> parseCube

parseColor :: Parser String
parseColor = parseString "red" <|> parseString "green" <|> parseString "blue"
  
possible :: Game -> Bool
possible (Game _ rounds) = not $ any impossible rounds
  where
    impossible (Round red green blue) = red > 12 || green > 13 || blue > 14

count :: [Game] -> Int
count games = sum $ map (no) (filter possible games)

parseNum :: Parser Int
parseNum = read <$> some (satisfy isDigit)

whitespace :: Parser String
whitespace = many (satisfy isSpace)

parseString :: String -> Parser String
parseString [x] = liftA2 (:) (satisfy (==x)) (pure [])
parseString (x:xs) = liftA2 (:) (satisfy (==x)) (parseString xs)

getGames :: [String] -> [Game]
getGames = map (\x -> case runParser parseGame x of 
  Nothing -> Game 0 []
  Just (g, _) -> g)

validGames :: [String] -> Int
validGames games = count $ getGames games

minimumCubes :: Game -> (Int, Int, Int)
minimumCubes (Game _ rounds) = (maximum redC, maximum greenC, maximum blueC)
  where
    redC = map red rounds
    greenC = map green rounds
    blueC = map blue rounds

power :: (Int, Int, Int) -> Int
power (r, g, b) = r*g*b

sumPowers :: [Game] -> Int
sumPowers games = sum $ map (power . minimumCubes) games

getGamePowers :: [String] -> Int
getGamePowers = sumPowers . getGames

solve :: IO ()
solve = do
  input <- readFile "02-Cube-Conundrum/input.txt"
  print $ validGames $ lines input
  print $ getGamePowers $ lines input 
