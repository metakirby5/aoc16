#!/usr/bin/env runhaskell
import Data.List.Split (splitOn)

type Coord = (Int, Int)
data Turn = L | R deriving (Read)
data Cardinal = N | E | S | W deriving (Enum)
data State = Position (Cardinal, Coord)
data Instruction = Move (Turn, Int)

start :: State
start = Position (N, (0, 0))

turn :: Cardinal -> Turn -> Cardinal
turn N L = W
turn W R = N
turn d t =
  case t of
    R -> succ d
    L -> pred d

walk :: State -> Int -> State
walk (Position (d, (x, y))) n = Position (d, c') where
  c' = case d of
         N -> (x, y + n)
         S -> (x, y - n)
         E -> (x + n, y)
         W -> (x - n, y)

move :: State -> Instruction -> State
move (Position (d, c)) (Move (t, n)) =
  walk (Position (turn d t, c)) n

dist :: State -> State -> Int
dist (Position (_, (x1, y1))) (Position (_, (x2, y2))) =
  (abs $ x1 - x2) + (abs $ y1 - y2)

readInstruction :: String -> Instruction
readInstruction (t:n) = Move (read [t], read n)

main :: IO ()
main = do
  raw <- getLine
  let instructions = map readInstruction $ splitOn ", " raw
  let end = foldl move start instructions
  print $ dist start end
