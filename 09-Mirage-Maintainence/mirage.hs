module Mirage () where

main :: IO () 
main = do
  input <- readFile "09-Mirage-Maintainence/input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 = sum . map (extrapolate . map read' . words) . lines

part2 :: String -> Int
part2 = sum . map (extrapolate . reverse . (map read') . words) . lines

read' :: String -> Int
read' ('-':xs) = negate $ read xs
read' xs = read xs

extrapolate :: [Int] -> Int
extrapolate [] = -1
extrapolate [x] = x
extrapolate l@(x:_)
  | allSame l = x
  | otherwise = last l + extrapolate (difference l)

difference :: [Int] -> [Int]
difference [] = []
difference [x] = []
difference (x:y:xs) = (y-x):(difference $ y:xs) 

allSame :: [Int] -> Bool
allSame [] = True
allSame (x:xs) = all (==x) xs
