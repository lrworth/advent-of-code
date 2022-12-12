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

data Direction = Up | Down | Left | Right
  deriving (Show)

parseDirection :: String -> Maybe Direction
parseDirection = \case
  "U" -> Just Up
  "D" -> Just Down
  "L" -> Just Left
  "R" -> Just Right
  _ -> Nothing

data Instruction = Instruction {direction :: Direction, distance :: Int}
  deriving (Show)

readInstructions :: String -> IO [Instruction]
readInstructions path = do
  ls <- lines <$> readFile' path
  Just instructions <- pure . forM ls $ \l -> do
    [dir, dist] <- pure $ words l
    direction <- parseDirection dir
    distance <- readMaybe dist
    pure Instruction {..}
  pure instructions

data Rope = Rope
  { head :: (Int, Int),
    tail :: (Int, Int)
  }
  deriving (Show)

moveDirection :: Direction -> Rope -> Rope
moveDirection direction Rope {head = (headX, headY), tail} =
  let newHead = case direction of
        Up -> (headX, headY + 1)
        Down -> (headX, headY - 1)
        Left -> (headX - 1, headY)
        Right -> (headX + 1, headY)
      newTail = moveTail newHead tail
   in Rope {head = newHead, tail = newTail}

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty) =
  if abs (hx - tx) >= 2 || abs (hy - ty) >= 2
    then (tx + signum (hx - tx), ty + signum (hy - ty))
    else (tx, ty)

applyDirection :: (MonadState Rope m, MonadWriter (Set (Int, Int)) m) => Direction -> m ()
applyDirection direction = do
  modify $ moveDirection direction
  tell . Set.singleton =<< gets tail

part1 :: IO Int
part1 = do
  is <- readInstructions "real.txt"
  Set.size
    <$> ( execWriterT $
            runStateT
              ( traverse
                  ( \Instruction {..} ->
                      replicateM distance $ applyDirection direction
                  )
                  is
              )
              Rope {head = (0, 0), tail = (0, 0)}
        )

newtype LongRope = LongRope (NonEmpty (Int, Int))
  deriving (Show)

tailForce :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailForce (hx, hy) (tx, ty) =
  if abs (hx - tx) >= 2 || abs (hy - ty) >= 2
    then (signum (hx - tx), signum (hy - ty))
    else (0, 0)

moveDirectionLong :: Direction -> LongRope -> LongRope
moveDirectionLong direction (LongRope nel) =
  LongRope $
    moveDirectionLong'
      ( case direction of
          Up -> (0, 1)
          Down -> (0, -1)
          Left -> (-1, 0)
          Right -> (1, 0)
      )
      nel
  where
    moveDirectionLong' :: (Int, Int) -> NonEmpty (Int, Int) -> NonEmpty (Int, Int)
    moveDirectionLong' offset (head :| tail) =
      let newHead = (getSum *** getSum $ bimap Sum Sum head <> bimap Sum Sum offset)
       in newHead :| case NEL.nonEmpty tail of
            Just tailNE@(subHead :| _) -> NEL.toList $ moveDirectionLong' (tailForce newHead subHead) tailNE
            Nothing -> []

applyDirectionLong :: (MonadState LongRope m, MonadWriter (Set (Int, Int)) m) => Direction -> m ()
applyDirectionLong direction = do
  modify $ moveDirectionLong direction
  tell . Set.singleton =<< gets (\(LongRope nel) -> NEL.last nel)

part2 :: IO Int
part2 = do
  is <- readInstructions "real.txt"
  Just startRope <- pure . fmap LongRope . NEL.nonEmpty $ replicate 10 (0, 0)
  Set.size
    <$> ( execWriterT $
            runStateT
              ( traverse
                  ( \Instruction {..} ->
                      replicateM distance $ applyDirectionLong direction
                  )
                  is
              )
              startRope
        )
