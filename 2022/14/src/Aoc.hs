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

data Object = Air | Rock | Sand
  deriving (Eq, Show)

data Cave = Cave
  { objects :: Map (Int, Int) Object,
    minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Show)

instance Semigroup Cave where
  a <> b = mkCave (objects a <> objects b)

mkCave :: Map (Int, Int) Object -> Cave
mkCave objects =
  let ((minX, maxX), (minY, maxY)) =
        (minimum &&& maximum) *** (minimum &&& maximum) $
          unzip $ Map.keys objects
   in Cave {..}

caveLookup :: Cave -> (Int, Int) -> Object
caveLookup Cave {..} (x, y) = fromMaybe Air $ Map.lookup (x, y) objects

newtype RockPath = RockPath [(Int, Int)]
  deriving (Show)

parseCoordinates :: String -> Maybe (Int, Int)
parseCoordinates raw = do
  [x, y] <- pure $ splitOn "," raw
  x' <- readMaybe x
  y' <- readMaybe y
  pure (x', y')

parseRockPath :: String -> Maybe RockPath
parseRockPath raw =
  RockPath <$> traverse parseCoordinates (splitOn " -> " raw)

sortPair :: Ord a => (a, a) -> (a, a)
sortPair (a, b) = if a <= b then (a, b) else (b, a)

wallCoordinatesFromCorners :: (Int, Int) -> (Int, Int) -> Maybe (Set (Int, Int))
wallCoordinatesFromCorners (fx, fy) (tx, ty) =
  let (xSmall, xLarge) = sortPair (fx, tx)
      (ySmall, yLarge) = sortPair (fy, ty)
   in if
          | fx == tx -> Just $ Set.fromList [(fx, y) | y <- [ySmall .. yLarge]]
          | fy == ty -> Just $ Set.fromList [(x, fy) | x <- [xSmall .. xLarge]]
          | otherwise -> Nothing

caveFromRockPath :: RockPath -> Maybe Cave
caveFromRockPath (RockPath corners) =
  fmap
    ( mkCave
        . Map.fromSet (const Rock)
        . Set.unions
    )
    . sequenceA
    $ zipWith wallCoordinatesFromCorners corners (tail corners)

readCaveFile :: String -> IO Cave
readCaveFile path = do
  Just caveLines <- NEL.nonEmpty . lines <$> readFile' path
  Just cavePaths <- pure . traverse parseRockPath $ caveLines
  Just caves <- pure $ traverse caveFromRockPath cavePaths
  pure $ sconcat caves

renderObject :: Object -> Char
renderObject = \case
  Air -> '.'
  Rock -> '#'
  Sand -> 'o'

renderCave :: Cave -> String
renderCave cave@Cave {..} =
  intercalate "\n" $
    ( \y ->
        (\x -> renderObject $ caveLookup cave (x, y))
          <$> [minX .. maxX]
    )
      <$> [minY .. maxY]

sandRestingPlace :: Cave -> (Int, Int) -> Maybe (Int, Int)
sandRestingPlace cave@Cave {..} (sandX, sandY)
  | sandY >= maxY = Nothing
  | otherwise =
    let down = (sandX, sandY + 1)
        downLeft = (sandX - 1, sandY + 1)
        downRight = (sandX + 1, sandY + 1)
     in if
            | caveLookup cave down == Air -> sandRestingPlace cave down
            | caveLookup cave downLeft == Air -> sandRestingPlace cave downLeft
            | caveLookup cave downRight == Air -> sandRestingPlace cave downRight
            | otherwise -> Just (sandX, sandY)

addSand :: Cave -> Maybe Cave
addSand c@Cave {..} = do
  (sandX, sandY) <- sandRestingPlace c (500, 0)
  pure Cave {objects = Map.insert (sandX, sandY) Sand objects, ..}

addLotsOfSand :: Cave -> Cave
addLotsOfSand c = case addSand c of
  Nothing -> c
  Just c' -> addLotsOfSand c'

part1 :: IO Int
part1 = do
  cave <- readCaveFile "real.txt"
  let filledCave = addLotsOfSand cave
  putStrLn $ renderCave filledCave
  pure . length . filter (== Sand) . Map.elems $ objects filledCave

caveLookup2 :: Cave -> (Int, Int) -> Object
caveLookup2 Cave {..} (x, y) = if y == maxY + 2 then Rock else fromMaybe Air $ Map.lookup (x, y) objects

sandRestingPlace2 :: Cave -> (Int, Int) -> (Int, Int)
sandRestingPlace2 cave (sandX, sandY) =
  let down = (sandX, sandY + 1)
      downLeft = (sandX - 1, sandY + 1)
      downRight = (sandX + 1, sandY + 1)
   in if
          | caveLookup2 cave down == Air -> sandRestingPlace2 cave down
          | caveLookup2 cave downLeft == Air -> sandRestingPlace2 cave downLeft
          | caveLookup2 cave downRight == Air -> sandRestingPlace2 cave downRight
          | otherwise -> (sandX, sandY)

addSand2 :: Cave -> Maybe Cave
addSand2 c@Cave {..} =
  let srp = sandRestingPlace2 c (500, 0)
   in if srp == (500, 0)
        then Nothing
        else Just Cave {objects = Map.insert srp Sand objects, ..}

addLotsOfSand2 :: Cave -> Cave
addLotsOfSand2 c@Cave {..} = case addSand2 c of
  Nothing -> Cave {objects = Map.insert (500, 0) Sand objects, ..}
  Just c' -> addLotsOfSand2 c'

renderCave2 :: Cave -> String
renderCave2 cave@Cave {..} =
  intercalate "\n" $
    ( \y ->
        (\x -> renderObject $ caveLookup2 cave (x, y))
          <$> [minX .. maxX]
    )
      <$> [minY .. maxY]

part2 :: IO Int
part2 = do
  cave <- readCaveFile "real.txt"
  let filledCave = addLotsOfSand2 cave
  putStrLn $ renderCave2 filledCave
  pure . length . filter (== Sand) . Map.elems $ objects filledCave
