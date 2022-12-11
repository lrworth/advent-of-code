{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.State
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
import Debug.Trace
import GHC.Generics hiding (to)
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty

readGrid :: String -> IO [[Natural]]
readGrid path = do
  ls <- lines <$> readFile' path
  Just grid <- pure $ (traverse . traverse) (readMaybe . (: "")) ls
  pure grid

-- Map each input number into True iff it was visible from the left.
visibleMask :: [Natural] -> [Bool]
visibleMask [] = []
visibleMask (outer : rest) =
  True :
  ( snd $
      mapAccumL
        (\waterline x -> (max x waterline, x > waterline))
        outer
        rest
  )

visibility :: [[Natural]] -> [[Bool]]
visibility grid =
  foldr
    (zipWith $ zipWith (||))
    (repeat $ repeat False)
    [fromLeft, fromRight, fromTop, fromBottom]
  where
    fromLeft = visibleMask <$> grid
    fromRight = reverse . visibleMask . reverse <$> grid
    fromTop = transpose $ visibleMask <$> transpose grid
    fromBottom = transpose $ reverse . visibleMask . reverse <$> transpose grid

part1 :: IO Int
part1 = do
  grid <- readGrid "real.txt"
  pure . sum . fmap (length . filter id) $ visibility grid

countVisible :: Natural -> [Natural] -> Int
countVisible height xs =
  let (lower, rest) = span (< height) xs
   in case rest of
        [] -> length lower
        _ -> length lower + 1

scenicScoresRightward :: [[Natural]] -> Maybe [[Int]]
scenicScoresRightward =
  traverse $
    traverse
      ( \case
          (x : xs) -> Just $ countVisible x xs
          _ -> Nothing
      )
      . init
      . tails

scenicScores :: [[Natural]] -> Maybe [[Int]]
scenicScores grid = do
  toRight <- scenicScoresRightward grid
  toLeft <- fmap (fmap reverse) . scenicScoresRightward $ fmap reverse grid
  toBottom <- fmap transpose . scenicScoresRightward $ transpose grid
  toTop <- fmap (transpose . fmap reverse) . scenicScoresRightward . fmap reverse $ transpose grid
  pure $
    foldr
      (zipWith $ zipWith (*))
      (repeat $ repeat 1)
      [toLeft, toRight, toTop, toBottom]

part2 :: IO Int
part2 = do
  grid <- readGrid "real.txt"
  Just scores <- pure $ scenicScores grid
  pure $ maximum $ fmap maximum scores
