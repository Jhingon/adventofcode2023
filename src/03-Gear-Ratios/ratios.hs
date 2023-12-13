module Ratios () where

import Parser
import Data.Char (isDigit)
import Data.List (tails)
import Control.Applicative
import Control.Monad (when, join)

data Thing = Symbol Int | Num Int (Int, Int) | Nope deriving (Show, Eq)

parseIntString :: Parser String
parseIntString = some (satisfy isDigit)

dots :: Parser String
dots = many $ satisfy (=='.')

dotsplus :: Parser String
dotsplus = many $ satisfy (`elem` s)
  where
    s = "!.@#$%&-+=/"

containsOneOf :: String -> String -> Bool
containsOneOf s1 s2 = foldl (\acc x -> acc || x `elem` s1) False s2 

symbol :: Int -> Parser Thing
symbol start = do
  ds <- dots
  _ <- satisfy (`elem` s)
  pure $ Symbol (start + length ds)
  where
    s = "!@#$%&*-+=/"

gear :: Int -> Parser Thing
gear start = do
  ds <- dotsplus
  _ <- satisfy (=='*')
  pure $ Symbol (start + length ds)

number :: Int -> Parser Thing
number start = do
  ds <- dots
  n <- parseIntString
  pure $ Num (read n) (start + length ds, start + length ds + length n - 1)

number' :: Int -> Parser Thing
number' start = do
  ds <- dotsplus
  n <- parseIntString
  pure $ Num (read n) (start + length ds, start + length ds + length n - 1)

nope :: Parser Thing
nope = do
  _ <- dots
  pure Nope

parseThings :: Int -> Parser [Thing]
parseThings start
  | start < 0 = pure []
  | otherwise = do 
    t <- (symbol start <|> number start <|> nope) 
    let newStart = getStart t
    ts <- parseThings newStart
    pure (t:ts)
    where
      getStart (Symbol n) = n + 1
      getStart (Num _ (_, n)) = n + 1 
      getStart Nope = -1

parseNumsAndGears :: Int -> Parser [Thing]
parseNumsAndGears start
  | start < 0 = pure []
  | otherwise = do 
    t <- (gear start <|> number' start <|> nope) 
    let newStart = getStart t
    ts <- parseNumsAndGears newStart
    pure (t:ts)
    where
      getStart (Symbol n) = n + 1
      getStart (Num _ (_, n)) = n + 1 
      getStart Nope = -1


dothething :: String -> [Int]
dothething input = findNums 0 (parseLines (parseThings 0) input)

parseLines :: Parser [Thing] -> String -> [[Thing]]
parseLines parser input = map (\x -> case runParser parser x of 
      Nothing -> error "poop"
      Just (t, _) -> t) (lines input)
    
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
findNums :: Int -> [[Thing]] -> [Int]
findNums index things
  | index < length things = map f t <> findNums (index+1) things
  | otherwise = []
  where
    handleMaybe (Just x) = x
    handleMaybe Nothing = []
    tprev = handleMaybe $ things !? (index - 1) 
    t = handleMaybe $ things !? index
    tnext = handleMaybe $ things !? (index + 1)
    f x@(Num n _) = if checkSides x t || checkUpDown x tprev || checkUpDown x tnext then n else 0
    f (Symbol n) = 0
    f Nope = 0

checkSides, checkUpDown :: Thing -> [Thing] -> Bool
checkSides (Symbol _) _ = False
checkSides Nope _ = False
checkSides (Num _ (n1, n2)) things = (Symbol (n1-1)) `elem` things || (Symbol (n2+1)) `elem` things
checkUpDown (Symbol _) _ = False
checkUpDown Nope _ = False
checkUpDown (Num _ (n1, n2)) things = foldl (\acc x -> acc || (Symbol x) `elem` things) False [(n1-1) .. (n2+1)]

dotheotherthing :: String -> Int
dotheotherthing input = sumGearRatios $ findGearNumbers 0 (parseLines (parseNumsAndGears 0) input)

findAdjacentNums :: Thing -> [Thing] -> [Thing] -> [Thing] -> (Int, Int)
findAdjacentNums (Nope) _ _ _ = (0,0)
findAdjacentNums (Num _ _) _ _ _ = (0,0)
findAdjacentNums (Symbol gear) tprev t tnext = if length result == 2 then (result !! 0, result !! 1) else (0,0)
  where
    up = convertToInt $ filter f tprev
    down = convertToInt $ filter f tnext
    sides = convertToInt $ filter g t
    result = sides <> up <> down
    convertToInt = filter (>=0) . map h 
    h Nope = -1
    h (Symbol _) = -1
    h (Num n _) = n
    f Nope = False
    f (Symbol _) = False
    f (Num _ (l, u)) = gear >= (l-1) && gear <= (u+1)
    g Nope = False
    g (Symbol _) = False
    g (Num _ (l, u)) = gear == l-1 || gear == u+1

findGearNumbers :: Int -> [[Thing]] -> [(Int, Int)]
findGearNumbers index things
  | index < length things = join (map f t) <> findGearNumbers (index+1) things
  | otherwise = []
  where
    handleMaybe (Just x) = x
    handleMaybe Nothing = []
    tprev = handleMaybe $ things !? (index - 1) 
    t = handleMaybe $ things !? index
    tnext = handleMaybe $ things !? (index + 1)
    adjacents s = findAdjacentNums s tprev t tnext
    f s@(Symbol n) = [adjacents s]
    f (Num _ _) = [(0,0)]
    f Nope = [(0,0)]

sumGearRatios :: [(Int, Int)] -> Int
sumGearRatios = sum . map (\(a,b) -> a * b)

solve :: IO ()
solve = do
  input <- readFile "03-Gear-Ratios/input.txt"
  print $ sum $ dothething input
  print "Part 2"
  print $ (parseLines (parseNumsAndGears 0) input)
  print $ filter (/=(0,0)) $ findGearNumbers 0 (parseLines (parseNumsAndGears 0) input)
  print $ dotheotherthing input
