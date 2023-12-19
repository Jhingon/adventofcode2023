{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Aplenty (main) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Parser

type Program = M.Map String Workflow

data Workflow = Workflow {conditions :: [Statement]} deriving (Show)

data Statement = Statement String Operator Int Statement | Return String | Accept | Reject deriving (Show)

data Operator = GreaterThan | LesserThan deriving (Eq, Show)

type Part = M.Map String Int

main :: IO ()
main = do
  programtext <- readFile "program.txt"
  input <- readFile "input.txt"
  let parts = parseInput input
  let program = parseP programtext
  mapM_ print (lines programtext)
  print (foldl (\acc x -> acc + sum (toList x)) 0 $ filter (eval program) parts)
  print ([(x,v) | (x,v) <- count program, x=="in"])

eval :: Program -> Part -> Bool
eval program part = go "in"
  where
    go atom = evalStatements statements
      where
        (Workflow statements) = program M.! atom
        evalStatements [] = error "Unexpected End of Input"
        evalStatements (x : xs) = case x of
          Statement attr op n next -> if execOp (part M.! attr) op n then evalStatements [next] else evalStatements xs
          Return atom' -> go atom'
          Accept -> True
          Reject -> False

execOp :: Int -> Operator -> Int -> Bool
execOp x GreaterThan y = x > y
execOp x LesserThan y = x < y

opposite :: Operator -> Operator
opposite GreaterThan = LesserThan
opposite LesserThan = GreaterThan

data ARanges = A {xmin :: Int, xmax :: Int, mmin :: Int, mmax :: Int, amin :: Int, amax :: Int, smin :: Int, smax :: Int} deriving (Show)

modifyRange :: String -> Operator -> Int -> ARanges -> ARanges
modifyRange name op n r = case name of
  "x" -> case op of
    GreaterThan -> if r.xmin < n then r {xmin = n + 1} else r
    LesserThan -> if r.xmax > n then r {xmax = n - 1} else r
  "m" -> case op of
    GreaterThan -> if r.mmin < n then r {mmin = n + 1} else r
    LesserThan -> if r.mmax > n then r {mmax = n - 1} else r
  "a" -> case op of
    GreaterThan -> if r.amin < n then r {amin = n + 1} else r
    LesserThan -> if r.amax > n then r {amax = n - 1} else r
  "s" -> case op of
    GreaterThan -> if r.smin < n then r {smin = n + 1} else r
    LesserThan -> if r.smax > n then r {smax = n - 1} else r
  _e -> error "oh no"

count program = map (\(x, y) -> (x, sum . map score $ y)) . map (\k -> let i = (k, count' initialA k) in i) $ (M.keys program)
  where
    initialA = A 1 4000 1 4000 1 4000 1 4000
    score (A xl xu ml mu al au sl su) = (abs (xu - xl) + 1) * (abs (ml - mu) + 1) * (abs (al - au) + 1) * (abs (sl - su) + 1)
    count' range x = fst $ foldl accumulate ([], range) statements
      where
        (Workflow statements) = program M.! x
        accumulate (foundRanges, currentRange) = \case
          Reject -> (foundRanges, currentRange)
          Accept -> (currentRange : foundRanges, currentRange)
          Return s -> (count' currentRange s <> foundRanges, currentRange)
          Statement name op n r ->
            let mr = modifyRange name op n currentRange
                mrop = modifyRange name (opposite op) n' currentRange
                n' = if op == GreaterThan then n + 1 else n - 1 -- flipping turns something into 'or equal to'
             in case r of
                  Reject -> (foundRanges, mrop)
                  Accept -> (mr : foundRanges, mrop)
                  Return s -> (count' mr s <> foundRanges, mrop)
                  _otherwise -> (foundRanges, currentRange)

parseInput :: String -> [Part]
parseInput = fst . fromJust . runParser parseParts

parseP :: String -> Program
parseP = fst . fromJust . runParser parseProgram

parseProgram :: Parser Program
parseProgram = M.fromList <$> some ((,) <$> some (satisfy (\x -> x /= '/' && isLetter x)) <*> parseWorkflow <* whitespace)

parseWorkflow :: Parser Workflow
parseWorkflow = Workflow <$> parseStatements

parseStatements :: Parser [Statement]
parseStatements = char '{' *> ((parseStatement <|> parseReturn) `sepBy` char ',') <* char '}'

parseReturn =
  (char 'A' $> Accept)
    <|> (char 'R' $> Reject)
    <|> (Return <$> some (satisfy isLetter))

parseStatement =
  Statement
    <$> some (satisfy isLetter)
    <*> parseOperator
    <*> parseInt
    <*> (char ':' *> parseReturn)

parseOperator :: Parser Operator
parseOperator = gt <|> lt
  where
    gt = char '>' $> GreaterThan
    lt = char '<' $> LesserThan

parseParts :: Parser [Part]
parseParts =
  some
    ( M.fromList
        <$> ( char '{'
                *> (((,) <$> (some (satisfy isLetter) <* char '=') <*> parseInt) `sepBy` char ',')
                <* char '}'
                <* whitespace
            )
    )
