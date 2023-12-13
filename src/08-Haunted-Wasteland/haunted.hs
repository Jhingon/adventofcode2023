module Pew (main) where

import Data.Char (isLetter, isDigit, isSpace)
import Data.List (find, sort, group, foldl1)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Parallel.Strategies

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser run) = Parser (\x -> fmap (first f) (run x))

instance Applicative Parser where
  pure a = Parser (\x -> Just (a, x))
  (Parser frun) <*> (Parser run) = Parser (\s -> do
    (f, s') <- frun s
    (a, s'') <- run s'
    pure (f a, s''))

instance Monad Parser where
  return = pure
  (Parser run) >>= f = Parser (\s -> do
    (a, s') <- run s
    let Parser run' = f a
    run' s')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ liftA2 (<|>) p1 p2

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs) = if p x then Just (x, xs) else Nothing

parseInt :: Parser Int
parseInt = read <$> many (satisfy isDigit)

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = liftA2 (:) (satisfy (==x)) (parseString xs)

whitespace :: Parser String
whitespace = many (satisfy isSpace)

sepBy :: Parser a -> Parser Char -> Parser [a]
p `sepBy` c = liftA2 (:) p (many rest)
  where
    rest = c *> p

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

main = solve'

solve :: IO ()
solve = do
  ex <- readFile "ex.txt"
  input <-readFile "input.txt"
  print $ part2 ex
  print $ part2 input

solve' = do
  input <- readFile "input.txt"
  part2' (parse input)

part1 input = move g (getNode nodes "AAA")
  where
    g@(Graph _ nodes) = parse input

part2 input = moveLikeAGhost (parse input)

part2' g@(Graph _ nodes) = do
  let new = map (schmove g) (filter (`endsWith` 'A') nodes)
  let pew = map head new
  print $ foldl1 lcm pew
  -- let answer = filter (\x -> length x > 1) $ group $ sort $ ns
  -- print answer

findSame :: [[Int]] -> [Int] -> IO [Int]
findSame _ [] = pure []
findSame xxs (n:ns) = do
  let xs@(y:ys) = map (!! n) xxs
  print $ show n <> ": " <> show xs
  (<>) <$> pure (xs) <*> findSame xxs ns

schmove :: Graph -> Node -> [Int]
schmove graph@(Graph steps nodes) node@(Node (current, (left, right)))
  = move' node (join $ repeat steps) 0
  where
    one (Node (_,(l,r))) s = case s of
      R -> getNode nodes r
      L -> getNode nodes l
    move' n@(Node (c, (l,r))) (s:ss) count = 
      if last c == 'Z' then count : move' (one n s) ss (count+1) else move' (one n s) ss (count+1)
    
endsWith (Node (n,_)) c = last n == c 

data Direction = L | R deriving (Show)
data Node = Node (String, (String, String)) deriving (Eq, Show)
data Graph = Graph [Direction] [Node] deriving (Show)

parseRL :: Parser [Direction]
parseRL = some $ (pure L <* satisfy (=='L')) <|> (pure R <* satisfy (=='R'))

parseLetters :: Parser String
parseLetters = some (satisfy (\x -> isLetter x || isDigit x))

parseNode :: Parser Node
parseNode
  =   (\n l r -> Node (n, (l,r)))
  <$> (parseLetters)
  <*> (whitespace *> satisfy (=='=') *> whitespace *> satisfy (=='(') *> parseLetters) 
  <*> (satisfy (==',') *> whitespace *> parseLetters <* satisfy(==')')) 

parseNodes :: Parser [Node]
parseNodes = parseNode `sepBy` (satisfy (=='\n'))

parseInput :: Parser Graph
parseInput = Graph <$> parseRL <*> (whitespace *> parseNodes)

parse :: String -> Graph
parse s = fst $ fromJust $ runParser parseInput s

getNode :: [Node] -> String -> Node
getNode ns name =fromJust $ find (\(Node (n,_)) -> n == name) ns

move :: Graph -> Node -> Int
move graph@(Graph steps nodes) node@(Node (current, (left, right)))
  = move' node (join $ repeat steps) 0
  where
    move' (Node (c, (l,r))) (s:ss) count
      | c == "ZZZ" = count
      | otherwise = case s of
        R -> move' (getNode nodes r) ss (count+1)
        L -> move' (getNode nodes l) ss (count+1)

moveLikeAGhost :: Graph -> Int
moveLikeAGhost graph@(Graph steps nodes)
  = ghost begins (join $ repeat steps) 0
  where
    begins = [x | x@(Node (n, _)) <- nodes, last n == 'A']
    one (Node (c, (l,r))) step = case step of
      R -> getNode nodes r
      L -> getNode nodes l
    allEndWith ns c = foldl (\acc n -> acc && (n `endsWith` c)) True ns 
    ghost starts (s:ss) count
      | starts `allEndWith` 'Z' = count
      | otherwise = ghost (parMap rpar (\x -> one x s) starts) ss (count+1)


