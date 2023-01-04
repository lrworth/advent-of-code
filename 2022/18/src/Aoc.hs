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
import Data.Semigroup (Max (Max), Min (Min))
import Data.Set qualified as Set

newtype Scan = Scan (Set (Int, Int, Int))
  deriving (Show)

data Cuboid = Cuboid
  { minPoint :: (Int, Int, Int),
    maxPoint :: (Int, Int, Int)
  }

boundingCuboid :: Scan -> Cuboid
boundingCuboid (Scan points) =
  Cuboid
    { minPoint = (minX, minY, minZ),
      maxPoint = (maxX, maxY, maxZ)
    }
  where
    ((Min minX, Min minY, Min minZ), (Max maxX, Max maxY, Max maxZ)) =
      foldMap (\(x, y, z) -> ((Min x, Min y, Min z), (Max x, Max y, Max z))) points

inflate :: Cuboid -> Cuboid
inflate Cuboid {minPoint = (minX, minY, minZ), maxPoint = (maxX, maxY, maxZ)} =
  Cuboid
    { minPoint = (pred minX, pred minY, pred minZ),
      maxPoint = (succ maxX, succ maxY, succ maxZ)
    }

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

floodFill :: Cuboid -> Scan -> Set (Int, Int, Int)
floodFill
  Cuboid
    { minPoint = minPoint@(minX, minY, minZ),
      maxPoint = (maxX, maxY, maxZ)
    }
  (Scan cubes) = go (Set.singleton minPoint) Set.empty
    where
      go :: Set (Int, Int, Int) -> Set (Int, Int, Int) -> Set (Int, Int, Int)
      go seeds visited =
        case Set.maxView seeds of
          Nothing -> visited
          Just ((x, y, z), remainingSeeds) ->
            let nextSeeds =
                  ( Set.fromList
                      [ (x', y', z')
                        | (x', y', z') <-
                            [ (succ x, y, z),
                              (pred x, y, z),
                              (x, succ y, z),
                              (x, pred y, z),
                              (x, y, succ z),
                              (x, y, pred z)
                            ],
                          minX <= x' && x' <= maxX,
                          minY <= y' && y' <= maxY,
                          minZ <= z' && z' <= maxZ
                      ]
                      `Set.union` remainingSeeds
                  )
                    `Set.difference` (visited `Set.union` cubes)
             in go nextSeeds (Set.insert (x, y, z) visited)

hull :: Set (Int, Int, Int) -> Set UnitFace
hull =
  Map.keysSet
    . Map.filter (== 1)
    . Map.unionsWith (+)
    . Set.map (Map.fromSet (const (1 :: Int)) . toFaces)

{-
1. Find a border containing all the cubes without touching any.
2. Flood-fill that box, not crossing any cube edges. The result will be another Set UnitFace.
2.1. Use an unfold algorithm. Seed with one of the corners. At each step, remember what cubes have already been produced and seed with all adjacent cubes that are not in that set.
3. Find the intersection between the flood-fill faces and the input faces.
-}
part2 :: IO Int
part2 = do
  scan@(Scan scanCubes) <- readInputFile "real.txt"
  let boundary = inflate . boundingCuboid $ scan
      outsideCubes = floodFill boundary scan
      outsideHull = hull outsideCubes
      scanHull = hull scanCubes
  pure $ Set.size $ scanHull `Set.intersection` outsideHull
