{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Control.Arrow
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.List (find, isPrefixOf, mapAccumL, sort, span, tails, transpose)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Generics hiding (to)
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty
import Prelude hiding (Left, Right, head, tail)

data Instruction = Noop | AddX Int
  deriving (Show)

parseInstruction :: String -> Maybe Instruction
parseInstruction l = case words l of
  "noop" : [] -> Just Noop
  "addx" : magnitude : [] -> AddX <$> readMaybe magnitude
  _ -> Nothing

readInstructions :: String -> IO [Instruction]
readInstructions path = do
  ls <- lines <$> readFile' path
  Just is <- pure $ traverse parseInstruction ls
  pure is

runInstruction :: (MonadWriter [Int] m, MonadState Int m) => Instruction -> m ()
runInstruction = \case
  Noop -> tell . (: []) =<< get
  AddX magnitude -> do
    tell . replicate 2 =<< get
    modify (+ magnitude)

cycleValues :: [Instruction] -> [Int]
cycleValues instructions =
  runIdentity $
    execWriterT $
      runStateT
        ( traverse runInstruction instructions
            *> (tell . (: []) =<< get)
        )
        1

part1 :: IO Int
part1 = do
  cv <- cycleValues <$> readInstructions "real.txt"
  pure
    . getSum
    . Map.foldMapWithKey (\k -> Sum . (* k))
    . Map.restrictKeys (Map.fromList . zip [1 :: Int ..] $ cv)
    $ Set.fromList [20, 60, 100, 140, 180, 220]

part2 :: IO ()
part2 = do
  cv <- cycleValues <$> readInstructions "real.txt"
  let crt = zipWith (\pixel sprite -> if abs ((pixel `mod` 40) - sprite) <= 1 then '#' else '.') [0 :: Int ..] cv
      crtLines = chunksOf 40 crt
  traceShowM $ length cv
  traverse_ putStrLn crtLines
