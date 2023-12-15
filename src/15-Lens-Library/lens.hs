module Lens (main) where

import Control.Applicative
import Data.Char
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as S
import Parser

type Lenses = M.Map Int (S.Seq (String, Int))

data Instruction = Add Int (String, Int) | Rem Int String deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ execute input

score :: Int -> [Int] -> Int
score boxnum lenses = (boxnum + 1) * (sum . zipWith (*) [1 ..] $ lenses)

hash :: String -> Int
hash = foldl' (\curr -> (`rem` 256) . (* 17) . (+ curr) . ord) 0

parseInstruction :: Lenses -> Parser Lenses
parseInstruction lm =
  ( (\(hash', name) values -> eval lm $ Add hash' (name, values))
      <$> (first <* char '=')
      <*> parseInt
  )
    <|> ( (\(key, name) -> eval lm $ Rem key name)
            <$> (first <* char '-')
        )
  where
    first = (\x -> (hash x, x)) <$> label
    label = some $ satisfy isAlpha

execute :: String -> Int
execute input =
  sum . map (\(boxnum, lensmap) -> score boxnum (map snd $ reverse $ toList lensmap)) . M.toList $
    fst $
      fromJust $
        until endofinput keeponsteppin (Just (lm, input))
  where
    lm :: Lenses
    lm = M.fromList [(i, S.empty :: S.Seq (String, Int)) | i <- [0 .. 255]]
    instruction x = parseInstruction x <|> char ',' *> parseInstruction x
    keeponsteppin (Just (lm', rest)) = runParser (instruction lm') rest
    keeponsteppin Nothing = Nothing
    endofinput Nothing = True
    endofinput x@(Just (_, nextc : _)) = isSpace nextc

eval :: Lenses -> Instruction -> Lenses
eval lm (Add k (label, val)) =
  let oldVal = lm M.! k
      i = S.findIndexL ((== label) . fst) oldVal
      newVal = case i of
        Nothing -> S.insertAt 0 (label, val) oldVal
        Just i' -> S.update i' (label, val) oldVal
   in M.insert k newVal lm
eval lm (Rem k label) =
  let oldVal = lm M.! k
      newVal = maybe oldVal (`S.deleteAt` oldVal) (S.findIndexL ((== label) . fst) oldVal)
   in M.insert k newVal lm
