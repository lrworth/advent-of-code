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
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.List (find, isPrefixOf, mapAccumL, sort, span, tails, transpose)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
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

newtype Coordinates = Coordinates (Int, Int)
  deriving (Eq, Ord, Show)

goLeft :: Coordinates -> Coordinates
goLeft (Coordinates (x, y)) = Coordinates (x - 1, y)

goRight :: Coordinates -> Coordinates
goRight (Coordinates (x, y)) = Coordinates (x + 1, y)

goUp :: Coordinates -> Coordinates
goUp (Coordinates (x, y)) = Coordinates (x, y - 1)

goDown :: Coordinates -> Coordinates
goDown (Coordinates (x, y)) = Coordinates (x, y + 1)

newtype Height = Height Char
  deriving (Enum, Eq, Ord, Show)

data Puzzle = Puzzle
  { start :: Coordinates,
    end :: Coordinates,
    heightMap :: Map Coordinates Height
  }
  deriving (Show)

readPuzzleFile :: String -> IO Puzzle
readPuzzleFile path = do
  ls <- lines <$> readFile' path
  let labelledInput :: [(Coordinates, Char)] =
        concat $
          zipWith
            ( \rowIx ->
                zipWith
                  ( \colIx ->
                      (Coordinates (colIx, rowIx),)
                  )
                  [0 ..]
            )
            [0 ..]
            ls
  Just (start, _) <- pure $ find (\(_, ch) -> ch == 'S') labelledInput
  Just (end, _) <- pure $ find (\(_, ch) -> ch == 'E') labelledInput
  let heightMap =
        Map.fromList $
          fmap
            ( fmap
                ( Height . \case
                    'S' -> 'a'
                    'E' -> 'z'
                    a -> a
                )
            )
            labelledInput
  pure Puzzle {..}

newtype Distance = Distance Int
  deriving (Enum, Eq, Ord, Show)

-- distancesFrom :: Coordinates -> Map Coordinates Height -> Map Coordinates Distance
-- distancesFrom start heightMap =
--   _ $
--     (`execStateT` (Distance (Map.size heightMap) <$ heightMap)) $ do
--       Just startHeight <- pure $ Map.lookup start heightMap
--       distanceMap <- get
--       Just startDistance <- pure $ Map.lookup start distanceMap
--       let nexts = do
--             coordinate <- [goLeft, goRight, goUp, goDown] ?? start
--             Just height <- pure $ Map.lookup coordinate heightMap
--             Just distance <- pure $ Map.lookup coordinate distanceMap
--             guard $ height <= succ startHeight
--             guard $ height <= succ startHeight
--             _
--       undefined

distancesFrom :: Map Coordinates Height -> Coordinates -> Map Coordinates Distance
distancesFrom heightMap start = shrinkWrap . Map.insert start (Distance 0) $ Distance (Map.size heightMap) <$ heightMap
  where
    shrinkWrap :: Map Coordinates Distance -> Map Coordinates Distance
    shrinkWrap initial =
      let surroundingReachableDistances :: Coordinates -> [Distance]
          surroundingReachableDistances coordinates = do
            Just height <- pure $ Map.lookup coordinates heightMap
            adjacent <- [goLeft, goRight, goUp, goDown] ?? coordinates
            Just adjacentHeight <- pure $ Map.lookup adjacent heightMap
            guard $ height <= succ adjacentHeight
            Just adjacentDistance <- pure $ Map.lookup adjacent initial
            pure adjacentDistance
          shrink :: Coordinates -> Distance -> Distance
          shrink coordinates distance =
            minimum $ distance : (succ <$> surroundingReachableDistances coordinates)
          shrunk = Map.mapWithKey shrink initial
       in if initial == shrunk then initial else shrinkWrap shrunk

part1 :: IO Distance
part1 = do
  Puzzle {..} <- readPuzzleFile "real.txt"
  Just d <- pure . Map.lookup end $ distancesFrom heightMap start
  pure d

distancesTo :: Map Coordinates Height -> Coordinates -> Map Coordinates Distance
distancesTo heightMap end = shrinkWrap . Map.insert end (Distance 0) $ Distance (Map.size heightMap) <$ heightMap
  where
    shrinkWrap :: Map Coordinates Distance -> Map Coordinates Distance
    shrinkWrap initial =
      let surroundingReachableDistances :: Coordinates -> [Distance]
          surroundingReachableDistances coordinates = do
            Just height <- pure $ Map.lookup coordinates heightMap
            adjacent <- [goLeft, goRight, goUp, goDown] ?? coordinates
            Just adjacentHeight <- pure $ Map.lookup adjacent heightMap
            guard $ adjacentHeight <= succ height
            Just adjacentDistance <- pure $ Map.lookup adjacent initial
            pure adjacentDistance
          shrink :: Coordinates -> Distance -> Distance
          shrink coordinates distance =
            minimum $ distance : (succ <$> surroundingReachableDistances coordinates)
          shrunk = Map.mapWithKey shrink initial
       in if initial == shrunk then initial else shrinkWrap shrunk

part2 :: IO Distance
part2 = do
  Puzzle {..} <- readPuzzleFile "real.txt"
  pure
    . minimum
    . Map.elems
    . Map.restrictKeys (distancesTo heightMap end)
    . Map.keysSet
    $ Map.filter (== Height 'a') heightMap
