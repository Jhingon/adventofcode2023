module Never (main) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Parser
import Z3.Monad

main :: IO ()
main = do
  input <- readFile "24-Never-Tell-Me-The-Odds/input.txt"
  let ls = lines input
  print (length . filter id . map (uncurry cramer) . pairs . map parse $ ls)

data Vector = Vector !Float !Float !Float deriving (Eq, Ord, Show)

type Position = Vector

type Velocity = Vector

pairs :: [a] -> [(a, a)]
pairs as = [(x, y) | (x : ys) <- tails as, y <- ys]

check :: (Position, Velocity) -> (Position, Velocity) -> Bool
check (Vector px1 py1 pz1, Vector vx1 vy1 vz1) (Vector px2 py2 pz2, Vector vx2 vy2 vz2) =
  let r1 = (px2 - px1) / (vx1 - vx2)
      r2 = (py2 - py1) / (vy1 - vy2)
      r3 = (pz2 - pz1) / (vz1 - vz2)
   in r1 == r2 && r2 == r3

cramer :: (Position, Velocity) -> (Position, Velocity) -> Bool
cramer (Vector px1 py1 _, Vector vx1 vy1 _) (Vector px2 py2 _, Vector vx2 vy2 _) =
  let a = (vx1, negate vx2, vy1, negate vy2)
      b1 = px2 - px1
      b2 = py2 - py1
      t1 = det (b1, negate vx2, b2, negate vy2) / det a
      t2 = det (vx1, b1, vy1, b2) / det a
      x1 = (px1 + vx1 * t1, py1 + vy1 * t1)
      x2 = (px2 + vx2 * t2, py2 + vy2 * t2)
   in traceShow (x1, x2) $
        det a /= 0
          && t1 > 0
          && t2 > 0
          && fst x1 > 200000000000000
          && fst x2 < 400000000000000
          && snd x1 > 200000000000000
          && snd x2 < 400000000000000

-- && ((py2 - py1) / (vy1 - vy2)) > 200000000000000
-- && ((py2 - py1) / (vy1 - vy2)) < 400000000000000
--
det :: (Float, Float, Float, Float) -> Float
det (a11, a12, a21, a22) = (a11 * a22) - (a12 * a21)

parse :: String -> (Vector, Vector)
parse = fst . fromJust . runParser parseLine

parseLine :: Parser (Vector, Vector)
parseLine = (,) <$> (parseVector <* whitespace <* char '@' <* whitespace) <*> parseVector

parseVector :: Parser Vector
parseVector =
  Vector
    <$> (parseNumber <* char ',' <* whitespace)
    <*> (parseNumber <* char ',' <* whitespace)
    <*> parseNumber

parseNumber :: Parser Float
parseNumber = fromIntegral <$> (parseInt <|> (char '-' *> fmap negate parseInt))
