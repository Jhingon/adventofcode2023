module Point (main) where

import Control.Applicative
import Data.List
import Data.Maybe
import Parser

main :: IO ()
main = do
  input <- readFile "input.txt"
  let ns = map part1 (parse input)
  print (part2 $ zip ns $ parse input)
  print (sum $ part2 $ zip ns $ parse input)

parseNotes :: Parser [[String]]
parseNotes = some ((some (satisfy (\x -> x == '#' || x == '.')) `sepBy` satisfy (== '\n')) <* parseString "\n\n")

parse :: String -> [[String]]
parse = fst . fromJust . runParser parseNotes

part1 :: [String] -> (Int, Int)
part1 ls =
  ( sum (map (\n -> if verticalR ls n then n else 0) [1 .. lx]),
    sum (map ((* 100) . \n -> if horizontalR ls n then n else 0) [1 .. ly])
  )
  where
    ly = length ls
    lx = length $ head ls

part2 = map fixSmudge

horizontalR :: [String] -> Int -> Bool
horizontalR ss n = before' == reverse after && (before /= [] && after /= [])
  where
    before = take n ss
    after = take n $ drop n ss
    before' = if length before > length after then drop (length before - length after) before else before

verticalR :: [String] -> Int -> Bool
verticalR ss = horizontalR (transpose ss)

replace :: [a] -> a -> Int -> [a]
replace xs a i = take i xs <> pure a <> drop (i + 1) xs

replace2 :: [[a]] -> a -> (Int, Int) -> [[a]]
replace2 xss a (i, j) = replace xss (replace (xss !! i) a j) i

fixSmudge ((nvert, nhor), ss) = fromMaybe (max nvert nhor) $ find (> 0) (newscores score)
  where
    lx = length ss
    ly = length $ head ss
    lxs = filter (/= (nhor `quot` 100)) [1 .. lx]
    lys = filter (/= nvert) [1 .. ly]
    coords =
      [ (x, y)
        | x <- [0 .. lx - 1],
          y <- [0 .. ly - 1]
      ]
    newscores sf = map (\(x, y) -> let n = replace2 ss (fl (ss !! x !! y)) (x, y) in sf n) coords
    fl '.' = '#'
    fl '#' = '.'
    scoreH x = sum $ map (\n -> if horizontalR x n then n * 100 else 0) lxs
    scoreL x = sum $ map (\n -> if verticalR x n then n else 0) lys
    score x
      | scoreH x > 0 = scoreH x
      | otherwise = scoreL x
