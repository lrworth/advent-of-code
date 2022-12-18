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
import qualified Data.Aeson as Aeson
import Data.Bitraversable
import qualified Data.ByteString.Lazy as ByteString
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.List (find, intercalate, intersperse, isPrefixOf, mapAccumL, sort, sortBy, span, tail, tails, transpose)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word ()
import Debug.Trace
import GHC.Generics hiding (to)
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty
import Prelude hiding (Left, Right, head, tail)

newtype Coordinates = Coordinates (Int, Int)
  deriving (Eq, Ord, Show)

data SensorData = SensorData {sensor :: Coordinates, beacon :: Coordinates}
  deriving (Show)

manhattanDistance :: Coordinates -> Coordinates -> Int
manhattanDistance (Coordinates (ax, ay)) (Coordinates (bx, by)) =
  abs (ax - bx) + abs (ay - by)

sensorRange :: SensorData -> Int
sensorRange SensorData {..} = manhattanDistance sensor beacon

parseLine :: String -> Maybe SensorData
parseLine raw = do
  [_, _, sxRaw, syRaw, _, _, _, _, bxRaw, byRaw] <- pure $ words raw
  sx <- readMaybe . drop 2 $ init sxRaw
  sy <- readMaybe . drop 2 $ init syRaw
  bx <- readMaybe . drop 2 $ init bxRaw
  by <- readMaybe $ drop 2 byRaw
  pure SensorData {sensor = Coordinates (sx, sy), beacon = Coordinates (bx, by)}

readInput :: String -> IO [SensorData]
readInput path = do
  Just sensors <- traverse parseLine . lines <$> readFile' path
  pure sensors

rowSensorEffect :: Int -> SensorData -> Set Coordinates
rowSensorEffect r sd@SensorData {..} =
  let range = sensorRange sd
      Coordinates (sx, sy) = sensor
      vDistance = abs $ r - sy
      hDistance = range - vDistance
   in Set.fromList $ Coordinates . (,r) <$> [sx - hDistance .. sx + hDistance]

removeBeacons :: [SensorData] -> Set Coordinates -> Set Coordinates
removeBeacons = appEndo . foldMap (Endo . Set.delete . beacon)

part1 :: IO Int
part1 = do
  sensors <- readInput "real.txt"
  pure . length . removeBeacons sensors . mconcat $ rowSensorEffect 2000000 <$> sensors

---- This doesn't eliminate anything
--eliminateImpossibleRows :: Int -> Int -> SensorData -> Set Int -> Set Int
--eliminateImpossibleRows minX maxX sd@SensorData {..} =
--  let range = sensorRange sd
--   in Set.filter
--        ( \y ->
--            range < manhattanDistance sensor (Coordinates (minX, y))
--              || range < manhattanDistance sensor (Coordinates (maxX, y))
--        )

eliminateImpossibleColumns :: Int -> SensorData -> RangeSet -> RangeSet
eliminateImpossibleColumns y sd@SensorData {..} =
  let range = sensorRange sd
      Coordinates (sx, sy) = sensor
      vDistance = abs $ y - sy
      hDistance = range - vDistance
   in if vDistance <= range
        then rangeSetDelete (sx - hDistance, sx + hDistance)
        else id

newtype RangeSet = RangeSet [(Int, Int)]
  deriving (Show)

rangeSetToList :: RangeSet -> [Int]
rangeSetToList (RangeSet ranges) = do
  (f, t) <- ranges
  [f .. t]

rangeSetNull :: RangeSet -> Bool
rangeSetNull (RangeSet ranges) = List.null ranges

rangeSetNew :: (Int, Int) -> RangeSet
rangeSetNew = RangeSet . (: [])

rangeSetDelete :: (Int, Int) -> RangeSet -> RangeSet
rangeSetDelete (df, dt) (RangeSet ranges) = RangeSet $ do
  (f, t) <- ranges
  if
      | df < f ->
        if
            | dt < f -> [(f, t)]
            | dt < t -> [(dt + 1, t)]
            | otherwise -> []
      | df == f ->
        if
            | dt < t -> [(dt + 1, t)]
            | otherwise -> []
      | df < t ->
        if
            | dt < t -> [(f, df - 1), (dt + 1, t)]
            | otherwise -> [(f, df - 1)]
      | df == t -> [(f, df - 1)]
      | otherwise -> [(f, t)]

part2 :: IO Int
part2 = do
  sensors <- readInput "real.txt"
  [(y, xs)] <-
    pure $
      filter (not . rangeSetNull . snd) $
        ( \y ->
            (y, appEndo (foldMap (Endo . eliminateImpossibleColumns y) sensors) $ rangeSetNew (0, 4000000))
        )
          <$> [0 .. 4000000]
  [x] <- pure $ rangeSetToList xs
  pure $ y + x * 4000000
