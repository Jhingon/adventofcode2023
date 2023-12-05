module Fertilizer () where

import Parser
import Data.List (find, foldl')
import Control.Monad (join)
import Control.Applicative
import Data.Char (isSpace)
import Data.Range
import Control.Parallel.Strategies

main = solve

solve :: IO ()
solve = do
  ex <- readFile "05-Fertilizer/ex.txt"
  input <- readFile "05-Fertilizer/input.txt"
  let x = runParser (parseInput) ex
  print x
  let y = (runParser parseInput input)
  print y

data M = M Int Int Int
data S = S Int Int deriving (Show, Eq, Ord)

generateMapping :: [M] -> (Int -> Int)
generateMapping ms = \n -> case find (within n) ms of
  Nothing -> n
  Just (M dest source _) -> dest + (n - source)

within :: Int -> M -> Bool
within n (M _ source range) = n >= source && n < source + range

parseMap :: String -> Parser [M]
parseMap m = parseString (m <> " map:\n") *> (some n) <* whitespace 
  where
    n = M <$> ((parseInt) <* whitespace) <*> (parseInt <* whitespace) <*> (parseInt <* satisfy(=='\n'))

parseSeeds :: Parser [S]
parseSeeds = parseString "seeds:" *> whitespace *> (pair `sepBy` (satisfy (==' ')))
  where
    pair = S <$> (parseInt <* whitespace) <*> parseInt

removeOverlapping :: [S] -> [Range Int]
removeOverlapping xs = (map (\(S a b) -> a +=+ (a+b-1)) xs)

f :: S -> [M] -> [S]
f seed [] = [seed]
f seed@(S a b) (M d s l : ms) = if overlap then before <> overlapping <> after else f seed ms
  where
    overlap = a <= s + l && a + b > s
    before = if a < s then f (S a (s-a)) ms else []
    overlapping = [S (d + newseedstart) (min b (l - newseedstart))]
    after = if s + l < a + b then f (S (s+l) (a + b - s - l)) ms else []
    newseedstart = max 0 (a-s)

f' :: Range Int -> [M] -> [Range Int]
f' ran [] = [ran]
f' ran@(SpanRange (Bound lower _) (Bound upper _)) (M d s l : ms) = if rangesOverlap ran sran' then notoverlapping <> overlapping else f' ran ms
  where
    sran' = s +=* (s + l)
    sran = [s +=* (s + l)]
    dran = [d +=* (d + l)]
    notoverlapping  = 
      if belowRange ran s 
      then f' (lower +=* s) ms 
      else if aboveRange ran s then f' ((s+l) +=* (upper)) ms else []
    overlapping = g (intersection [ran] sran)
    g [] = []
    g rs = let (upper,lower) = (head $ fromRanges rs, last $ fromRanges rs) in [(d + max 0 (lower - s)) +=* (if upper < s + l then upper else s + l)]
    g [(SpanRange (Bound lower _) (Bound upper _))] = [(d + max 0 (lower - s)) +=* (if upper < s + l then upper else s + l)]

parseInput = do
  s <- parseSeeds
  whitespace
  m1 <- parseMap "seed-to-soil"
  whitespace
  m2 <- parseMap "soil-to-fertilizer"
  whitespace
  m3 <- parseMap "fertilizer-to-water"
  whitespace
  m4 <- parseMap "water-to-light"
  whitespace
  m5 <- parseMap "light-to-temperature"
  whitespace
  m6 <- parseMap "temperature-to-humidity"
  whitespace
  m7 <- parseMap "humidity-to-location"
  let ms = [m1 , m2 , m3 , m4 , m5 , m6 , m7]
  let s' = removeOverlapping s
  let pew = minimum $ map (\(S a _ ) -> a) $ foldl' (\acc x -> join $ map (\z -> f z x) acc) s ms 
  pure pew
  -- let locs = withStrategy (parBuffer 10000000 rdeepseq) $ map loc seeds
  -- pure locs

