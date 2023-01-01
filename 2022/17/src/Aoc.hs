{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Aoc where

import Data.Foldable (Foldable (maximum), maximumBy)
import Data.Set qualified as Set
import Relude.Extra.Foldable1 (Foldable1 (..), maximumOn1)
import Relude.Extra.Tuple (fmapToSnd)
import Relude.Unsafe qualified as Unsafe

data Jet = L | R
  deriving (Show)

readInputFile :: FilePath -> IO [Jet]
readInputFile path = do
  [jetPatternRaw] <- lines . decodeUtf8 <$> readFileBS path
  Just jetPattern <-
    pure
      $ traverse
        ( \case
            '<' -> Just L
            '>' -> Just R
            _ -> Nothing
        )
      $ toString jetPatternRaw
  pure jetPattern

newtype Rock
  = Rock
      ( -- Locations of all rock parts. Positive coordinates are rightward and upward.
        Set (Int, Int)
      )
  deriving (Show)

rocks :: [Rock]
rocks =
  Rock . Set.fromList
    <$> [ [(0, 0), (1, 0), (2, 0), (3, 0)],
          [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
          [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
          [(0, 0), (0, 1), (0, 2), (0, 3)],
          [(0, 0), (1, 0), (0, 1), (1, 1)]
        ]

data Chamber = Chamber
  { landedRock :: Rock,
    jets :: [Jet],
    fallingRocks :: [Rock],
    fallingRockCoord :: (Int, Int)
  }
  deriving (Show)

rockHeight :: Rock -> Int
rockHeight (Rock parts) =
  maybe 0 ((+ 1) . maximum1)
    . nonEmpty
    $ snd <$> toList parts

overlaps :: Rock -> Rock -> Bool
overlaps (Rock a) (Rock b) = not $ Set.disjoint a b

joinRocks :: Rock -> Rock -> Rock
joinRocks (Rock a) (Rock b) = Rock $ Set.union a b

applyJet :: Jet -> Int -> Int
applyJet = \case
  L -> (subtract 1)
  R -> (+ 1)

rockApplyOffset :: (Int, Int) -> Rock -> Rock
rockApplyOffset (offsetX, offsetY) (Rock parts) =
  Rock $ Set.map (\(x, y) -> (x + offsetX, y + offsetY)) parts

rockWidth :: Rock -> Int
rockWidth (Rock parts) = 1 + maximum (Set.map fst parts)

currentFallingRock :: Chamber -> Rock
currentFallingRock Chamber {fallingRocks} = maybe (Unsafe.head rocks) head $ nonEmpty fallingRocks

nextFallingRocks :: Chamber -> [Rock]
nextFallingRocks Chamber {fallingRocks} = maybe (Unsafe.tail rocks) tail $ nonEmpty fallingRocks

chamberApplyJet :: [Jet] -> Chamber -> Chamber
chamberApplyJet jetPattern c@Chamber {landedRock, jets, fallingRockCoord} =
  if fallingRockNewX < 0 || (fallingRockNewX + fallingRockWidth) > 7 || overlaps landedRock fallingRockAbsolute
    then c {jets = nextJets}
    else c {fallingRockCoord = fallingRockNewCoord, jets = nextJets}
  where
    nextJets = maybe (Unsafe.tail jetPattern) tail $ nonEmpty jets
    jet = maybe (Unsafe.head jetPattern) head $ nonEmpty jets
    fallingRockNewCoord@(fallingRockNewX, _) = first (applyJet jet) fallingRockCoord
    fallingRockAbsolute = rockApplyOffset fallingRockNewCoord $ currentFallingRock c
    fallingRockWidth = rockWidth $ currentFallingRock c

landRock :: Chamber -> Chamber
landRock c@Chamber {landedRock, fallingRockCoord} =
  c
    { landedRock = newLandedRock,
      fallingRocks = nextFallingRocks c,
      fallingRockCoord = (2, rockHeight newLandedRock + 3)
    }
  where
    newLandedRock =
      joinRocks landedRock
        . rockApplyOffset fallingRockCoord
        $ currentFallingRock c

chamberApplyGravity :: Chamber -> (Chamber, Bool)
chamberApplyGravity c@Chamber {landedRock, fallingRockCoord} =
  if fallingRockNewY < 0 || overlaps landedRock fallingRockAbsolute
    then (landRock c, True)
    else (c {fallingRockCoord = fallingRockNewCoord}, False)
  where
    fallingRockNewCoord@(_, fallingRockNewY) = second (subtract 1) fallingRockCoord
    fallingRockAbsolute = rockApplyOffset fallingRockNewCoord (currentFallingRock c)

part1 :: IO Int
part1 = do
  jetPattern <- readInputFile "sample.txt"
  let initialChamber =
        Chamber
          { landedRock = Rock Set.empty,
            jets = jetPattern,
            fallingRocks = rocks,
            fallingRockCoord = (2, 3)
          }
  finalRock <- landedRock <$> go jetPattern (2022 :: Integer) initialChamber
  -- putTextLn $ drawRock finalRock
  pure $ rockHeight finalRock
  where
    go jetPattern landingsLeft chamber = do
      -- print landingsLeft
      if landingsLeft == 0
        then pure chamber
        else
          let (newChamber, landed) = chamberApplyGravity $ chamberApplyJet jetPattern chamber
           in do
                -- print $ fallingRockCoord newChamber
                -- putTextLn . drawRock $ currentFallingRock newChamber
                -- putTextLn . drawRock $ landedRock newChamber
                go jetPattern (if landed then landingsLeft - 1 else landingsLeft) newChamber

drawRock :: Rock -> Text
drawRock r@(Rock parts) =
  unlines . fmap toText $ (drawRow <$> [height, height - 1 .. 0]) <> ["+" <> replicate 7 '-' <> "+"]
  where
    height = rockHeight r
    drawRow y = "|" <> ((\x -> if Set.member (x, y) parts then '#' else '.') <$> [0 .. 6]) <> "|"

-- part2 :: IO Int
-- part2 = do
--  jetPattern <- readInputFile "sample.txt"
--  let initialChamber =
--        Chamber
--          { landedRock = Rock Set.empty,
--            jets = cycle jetPattern,
--            fallingRocks = cycle rocks,
--            fallingRockCoord = (2, 3)
--          }
--  let finalRock = find ((== initialChamber) . snd) $ fmapToSnd (\n -> go (n :: Integer) initialChamber) [1 ..]
--  undefined
--  where
--    go landingsLeft chamber = do
--      -- print landingsLeft
--      if landingsLeft == 0
--        then chamber
--        else
--          let (newChamber, landed) = chamberApplyGravity $ chamberApplyJet chamber
--           in do
--                -- print $ fallingRockCoord newChamber
--                -- putTextLn . drawRock $ landedRock newChamber
--                go (if landed then landingsLeft - 1 else landingsLeft) newChamber
