module Parser (
  Parser (..),
  satisfy,
  parseInt,
  parseString,
  sepBy,
  whitespace
) where

import Data.Char (isDigit, isSpace)
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser run) = Parser (fmap (first f) . run)

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
