{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Data.List (nub)
import System.IO (readFile')

-- Old version
-- uniquePosition4 :: String -> Int
-- uniquePosition4 (a : b : c : d : rest) =
--   if nub [a, b, c, d] == [a, b, c, d]
--     then 4
--     else 1 + uniquePosition4 (b : c : d : rest)

uniquePosition :: Int -> String -> Int
uniquePosition n cs =
  let front = take n cs
   in if nub front == front
        then n
        else 1 + uniquePosition n (tail cs)

part1 :: IO Int
part1 = do
  input <- readFile' "real.txt"
  pure $ uniquePosition 4 input

part2 :: IO Int
part2 = do
  input <- readFile' "real.txt"
  pure $ uniquePosition 14 input
