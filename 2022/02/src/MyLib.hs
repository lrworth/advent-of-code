{-# LANGUAGE LambdaCase #-}

module MyLib where

import Debug.Trace
import Numeric.Natural (Natural)
import System.IO (readFile')

data Shape = Rock | Paper | Scissors
  deriving (Show)

shapeScore :: Shape -> Natural
shapeScore = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

data Outcome = Win | Lose | Tie
  deriving (Show)

play :: Shape -> Shape -> Outcome
play =
  \cases
   Rock Rock -> Tie
   Rock Paper -> Lose
   Rock Scissors -> Win
   Paper Rock -> Win
   Paper Paper -> Tie
   Paper Scissors -> Lose
   Scissors Rock -> Lose
   Scissors Paper -> Win
   Scissors Scissors -> Tie

score :: Outcome -> Natural
score = \case
  Win -> 6
  Lose -> 0
  Tie -> 3

playScore :: Shape -> Shape -> Natural
playScore challenge response = shapeScore response + score (play response challenge)

parseStrategyLine :: String -> Maybe (Shape, Shape)
parseStrategyLine s = do
  [challenge, ' ', response] <- pure s
  c <- case challenge of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissors
    _ -> Nothing
  r <- case response of
    'X' -> Just Rock
    'Y' -> Just Paper
    'Z' -> Just Scissors
    _ -> Nothing
  pure (c, r)

part1 :: IO Natural
part1 = do
  Just strategy <- traverse parseStrategyLine . lines <$> readFile' "real.txt"
  pure $ sum $ uncurry playScore <$> strategy

parseStrategyLine2 :: String -> Maybe (Shape, Outcome)
parseStrategyLine2 s = do
  [challenge, ' ', outcome] <- pure s
  c <- case challenge of
    'A' -> Just Rock
    'B' -> Just Paper
    'C' -> Just Scissors
    _ -> Nothing
  o <- case outcome of
    'X' -> Just Lose
    'Y' -> Just Tie
    'Z' -> Just Win
    _ -> Nothing
  pure (c, o)

chooseShape :: Shape -> Outcome -> Shape
chooseShape = \cases
  Rock Win -> Paper
  Rock Lose -> Scissors
  Rock Tie -> Rock
  Paper Win -> Scissors
  Paper Lose -> Rock
  Paper Tie -> Paper
  Scissors Win -> Rock
  Scissors Lose -> Paper
  Scissors Tie -> Scissors

playScore2 :: Shape -> Outcome -> Natural
playScore2 challenge outcome =
    playScore challenge (chooseShape challenge outcome)

part2 :: IO Natural
part2 = do
  Just strategy <- traverse parseStrategyLine2 . lines <$> readFile' "real.txt"
  pure $ sum $ uncurry playScore2 <$> strategy
