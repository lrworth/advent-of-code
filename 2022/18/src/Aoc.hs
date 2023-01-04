{-# LANGUAGE ApplicativeDo #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Aoc where

import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Set qualified as Set

newtype Scan = Scan (Set (Int, Int, Int))
  deriving (Show)

readScanLine :: Text -> Maybe (Int, Int, Int)
readScanLine line = do
  [x, y, z] <- pure $ splitOn "," $ toString line
  x' <- readMaybe x
  y' <- readMaybe y
  z' <- readMaybe z
  pure (x', y', z')

readInputFile :: FilePath -> IO Scan
readInputFile path = do
  Just scanLines <- traverse readScanLine . lines . decodeUtf8 <$> readFileBS path
  pure . Scan $ Set.fromList scanLines

data Axis = X | Y | Z
  deriving (Eq, Ord, Show)

data UnitFace = UnitFace
  { normal :: Axis,
    minPoint :: (Int, Int, Int)
  }
  deriving (Eq, Ord, Show)

toFaces :: (Int, Int, Int) -> Set UnitFace
toFaces (x, y, z) =
  Set.fromList
    [ UnitFace {normal = X, minPoint = (x + 1, y, z)},
      UnitFace {normal = X, minPoint = (x, y, z)},
      UnitFace {normal = Y, minPoint = (x, y + 1, z)},
      UnitFace {normal = Y, minPoint = (x, y, z)},
      UnitFace {normal = Z, minPoint = (x, y, z + 1)},
      UnitFace {normal = Z, minPoint = (x, y, z)}
    ]

faceOccurrences :: Scan -> Map UnitFace Int
faceOccurrences (Scan cubes) = Map.unionsWith (+) $ Set.map (Map.fromSet (const 1) . toFaces) cubes

part1 :: IO Int
part1 = do
  scan <- readInputFile "real.txt"
  pure . Map.size . Map.filter (< 2) $ faceOccurrences scan

part2 :: IO Int
part2 = undefined
