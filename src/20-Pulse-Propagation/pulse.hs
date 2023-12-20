{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Pulse (main) where

import Control.Applicative
import qualified Data.Array as A
import Data.Char
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Parser

data State = On | Off deriving (Eq, Show)

data FlipFlop = FlipFlop State deriving (Eq, Show)

data Conjunction = Conjunction (A.Array Int State) [(Int, String)] deriving (Eq, Show)

data Broadcaster = Broadcaster State deriving (Eq, Show)

data Module = F FlipFlop | C Conjunction | B Broadcaster deriving (Eq, Show)

type Circuit = M.Map String (Module, [String])

main :: IO ()
main = do
  input <- readFile "input.txt"
  mapM_ print (lines input)
  let c = initConjunctions $ fst $ fromJust (runParser parseCircuit input)
  let pushButtons = replicate 10000 (Off, "broadcaster")
  print (eval c "rd" pushButtons)

lcms [] = 1
lcms (x : xs) = lcm x (lcms xs)

eval :: Circuit -> String -> [(State, String)] -> (Int, Int)
eval circuit findOn nexts =
  snd $
    foldl
      ( \(c, acc) (i, j) -> traceShow "Button Press" $ go (c, acc) [(i, j, "")]
      )
      (circuit, (0, 0))
      nexts
  where
    go :: (Circuit, (Int, Int)) -> [(State, String, String)] -> (Circuit, (Int, Int))
    go fin [] = fin
    go (circ, acc) ns = go (circ', acc') next'
      where
        (circ', next) = foldl f (circ, []) ns
        next' = filter (\(_, j, _) -> j /= "output") next
        acc' =
          foldl sumTuple acc $
            map (\(state, n, prevn) -> if state == Off && prevn == findOn then (1, 0) else (0, 0)) next
        f (c, xs) (state, n, prevn) =
          (\x -> if prevn == "fv" then traceShow (state, n, prevn) x else x) $
            if n == "output"
              then (c, pure (state, n, prevn) <> xs)
              else
                if M.notMember n c
                  then (c, xs)
                  else
                    let (m, ns') = c M.! n
                     in case m of
                          F x ->
                            let newF@(FlipFlop x') = runFlipFlop x state
                             in if state == Off
                                  then (M.insert n (F newF, ns') c, map (x',,n) ns' <> xs)
                                  else (c, xs)
                          C x ->
                            let newC@(Conjunction x' _) = runConjunction x state prevn
                                x'' = A.elems x'
                                out = if all (== On) x'' then Off else On
                             in (M.insert n (C newC, ns') c, map (out,,n) ns' <> xs)
                          B _ -> (c, map (Off,,n) ns' <> xs)

initConjunctions :: Circuit -> Circuit
initConjunctions circuit =
  let allConj = filter conjOnly clist
      clist = M.assocs circuit
      conjOnly (_, (C _, _)) = True
      conjOnly _ = False
      circuit' = foldl f circuit allConj
      f c (name, (C (Conjunction _ _), ns)) = M.insert name ((C $ Conjunction newarr connections), ns) c
        where
          connections = zipWith (\i (n, _) -> (i, n)) [0 ..] . filter (\(_, (_, conns)) -> name `elem` conns) $ clist
          numConn = length connections
          newarr = A.listArray (0, numConn - 1) (replicate numConn Off)
   in circuit'

sumTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuple (a, b) (a', b') = (a + a', b + b')

runConjunction :: Conjunction -> State -> String -> Conjunction
runConjunction (Conjunction state list) newstate name = Conjunction state' list
  where
    state' = state A.// [(i, newstate)]
    i = fst . fromJust $ find ((== name) . snd) list

opp :: State -> State
opp On = Off
opp Off = On

runFlipFlop :: FlipFlop -> State -> FlipFlop
runFlipFlop (FlipFlop s) = \case
  On -> FlipFlop s
  Off -> FlipFlop $ opp s

insertAt :: Int -> a -> [a] -> [a]
insertAt n a xs = take n xs <> pure a <> drop (n + 1) xs

parseCircuit :: Parser Circuit
parseCircuit = M.fromList <$> many parseLine

parseLine :: Parser (String, (Module, [String]))
parseLine = (\(mod, name) dest -> (name, (mod, dest))) <$> parseModule <*> parseList <* whitespace

parseModule :: Parser (Module, String)
parseModule =
  (parseString "broadcaster" $> (B $ Broadcaster Off, "broadcaster"))
    <|> ((,) <$> (flipflop <|> conj) <*> some (satisfy isLetter))
  where
    flipflop = char '%' $> F (FlipFlop Off)
    conj = char '&' $> C (Conjunction (A.array (0, 0) []) [])

parseList :: Parser [String]
parseList = whitespace *> parseString "->" *> whitespace *> ((whitespace *> some (satisfy isLetter)) `sepBy` char ',')
