{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit, isAlpha)
import Control.Applicative

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

parseDigit :: Parser Char
parseDigit = satisfy isDigit

parseNum :: String -> Char -> Parser Char
parseNum [] num = empty
parseNum [x] num = do
  _ <- satisfy (==x)
  pure num
parseNum (x:xs) num = do
  _ <- satisfy (==x)
  parseNum xs num

parseOne :: Parser Char
parseOne = parseNum "one" '1'

parseTwo :: Parser Char
parseTwo = parseNum "two" '2'

parseThree :: Parser Char
parseThree = parseNum "three" '3'

parseFour :: Parser Char
parseFour = parseNum "four" '4'

parseFive :: Parser Char
parseFive = parseNum "five" '5'

parseSix :: Parser Char
parseSix = parseNum "six" '6'

parseSeven :: Parser Char
parseSeven = parseNum "seven" '7'

parseEight :: Parser Char
parseEight = parseNum "eight" '8'

parseNine :: Parser Char
parseNine = parseNum "nine" '9'

one :: Parser Char
one = parseOne <|> satisfy (=='1')

two :: Parser Char
two = parseTwo <|> satisfy (=='2')

three :: Parser Char
three = parseThree <|> satisfy (=='3')

four :: Parser Char
four = parseFour <|> satisfy (=='4')

five :: Parser Char
five = parseFive <|> satisfy (=='5')

six :: Parser Char
six = parseSix <|> satisfy (=='6')

seven :: Parser Char
seven = parseSeven <|> satisfy (=='7')

eight :: Parser Char
eight = parseEight <|> satisfy (=='8')

nine :: Parser Char
nine = parseNine <|> satisfy (=='9')

eno :: Parser Char
eno = parseNum "eno" '1' <|> satisfy (=='1')

owt :: Parser Char
owt = parseNum "owt" '2' <|> satisfy (=='2')

eerht :: Parser Char
eerht = parseNum "eerht" '3' <|> satisfy (=='3')

ruof :: Parser Char
ruof = parseNum "ruof" '4' <|> satisfy (=='4')

evif :: Parser Char
evif = parseNum "evif" '5' <|> satisfy (=='5')

xis :: Parser Char
xis = parseNum "xis" '6' <|> satisfy (=='6')

neves :: Parser Char
neves = parseNum "neves" '7' <|> satisfy (=='7')

thgie :: Parser Char
thgie = parseNum "thgie" '8' <|> satisfy (=='8')

enin :: Parser Char
enin = parseNum "enin" '9' <|> satisfy (=='9')

parseFirstNum :: Parser Char
parseFirstNum = 
  (one <|> two <|> three <|> four <|> five <|> six <|> seven <|> eight <|> nine) <|> (satisfy isAlpha *> parseFirstNum)

parseLastNum :: Parser Char
parseLastNum = (eno <|> owt <|> eerht <|> ruof <|> evif <|> xis <|> neves <|> thgie <|> enin) <|> (satisfy isAlpha *> parseLastNum)

getNumbers :: String -> [Int]
getNumbers s = map (convert) (lines s)
  where
    num1 x = runParser parseFirstNum x
    num2 x = runParser parseLastNum (reverse x)
    convert' x = do
      (n1, _) <- num1 x
      (n2, _) <- num2 x
      pure $ read (n1:n2:[])
    convert x = case convert' x of
      Nothing -> 0
      Just x  -> x


first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

solve :: IO ()
solve = do 
  input <- readFile "../input.txt"
  print $ getNumbers input
  print $ sum (getNumbers input)
