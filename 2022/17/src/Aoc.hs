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

import Data.Finite (Finite, getFinite, modulo)
import Data.Foldable (Foldable (maximum), minimum)
import Data.Set qualified as Set
import Data.Vector.Sized qualified as V
import Relude.Extra.Foldable1 (Foldable1 (..))

data Jet = L | R
  deriving (Eq, Show)

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
  deriving (Eq, Show)

keepTop :: Rock -> Rock
keepTop r@(Rock parts) = case bottom of
  Nothing -> r
  Just minY -> Rock $ Set.filter (\(_, y) -> y >= minY) parts
  where
    bottom = find (\y -> all (\x -> Set.member (x, y) parts) [0 .. 6]) [rockHeight r - 1, rockHeight r - 2 .. 0]

rocks :: V.Vector 5 Rock
rocks =
  Rock . Set.fromList
    <$> V.fromTuple
      ( [(0, 0), (1, 0), (2, 0), (3, 0)],
        [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
        [(0, 0), (0, 1), (0, 2), (0, 3)],
        [(0, 0), (1, 0), (0, 1), (1, 1)]
      )

grounded :: Rock -> Rock
grounded (Rock parts) = Rock $ Set.map (\(x, y) -> (x, y - minY)) parts
  where
    minY = minimum $ Set.map snd parts

data Cycle n a = Cycle
  { source :: V.Vector n a,
    index :: Finite n
  }
  deriving (Eq, Show)

mkCycle :: KnownNat n => V.Vector n a -> Cycle n a
mkCycle source = Cycle {source, index = 0}

peek :: KnownNat n => Cycle n a -> a
peek Cycle {source, index} = V.index source index

pop :: KnownNat n => State (Cycle n a) a
pop = do
  c@Cycle {index} <- get
  put $ c {index = (modulo . succ . getFinite) index}
  pure $ peek c

data Chamber nj nr = Chamber
  { landedRock :: Rock,
    jets :: Cycle nj Jet,
    fallingRocks :: Cycle nr Rock,
    fallingRockCoord :: (Int, Int)
  }
  deriving (Eq, Show)

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

currentFallingRock :: KnownNat nr => Chamber nj nr -> Rock
currentFallingRock Chamber {fallingRocks} = peek fallingRocks

chamberApplyJet :: (KnownNat nj, KnownNat nr) => State (Chamber nj nr) ()
chamberApplyJet = modify $ \c@Chamber {landedRock, jets, fallingRockCoord} ->
  let (jet, nextJets) = runState pop jets
      fallingRockNewCoord@(fallingRockNewX, _) = first (applyJet jet) fallingRockCoord
      fallingRockAbsolute = rockApplyOffset fallingRockNewCoord $ currentFallingRock c
      fallingRockWidth = rockWidth $ currentFallingRock c
   in if fallingRockNewX < 0 || (fallingRockNewX + fallingRockWidth) > 7 || overlaps landedRock fallingRockAbsolute
        then c {jets = nextJets}
        else c {fallingRockCoord = fallingRockNewCoord, jets = nextJets}

landRock :: (KnownNat nr) => State (Chamber nj nr) ()
landRock = modify $ \c@Chamber {landedRock, fallingRocks, fallingRockCoord} ->
  let (rock, nextRocks) = runState pop fallingRocks
      newLandedRock =
        joinRocks
          landedRock
          (rockApplyOffset fallingRockCoord rock)
   in c
        { landedRock = keepTop newLandedRock,
          fallingRocks = nextRocks,
          fallingRockCoord = (2, rockHeight newLandedRock + 3)
        }

chamberApplyGravity :: (KnownNat nr) => State (Chamber nj nr) Bool
chamberApplyGravity = do
  c@Chamber {landedRock, fallingRockCoord} <- get
  let fallingRockNewCoord@(_, fallingRockNewY) = second (subtract 1) fallingRockCoord
      fallingRockAbsolute = rockApplyOffset fallingRockNewCoord (currentFallingRock c)
  if fallingRockNewY < 0 || overlaps landedRock fallingRockAbsolute
    then True <$ landRock
    else False <$ put (c {fallingRockCoord = fallingRockNewCoord})

drawRock :: Rock -> Text
drawRock r@(Rock parts) =
  unlines . fmap toText $ (drawRow <$> [height, height - 1 .. 0]) <> ["+" <> replicate 7 '-' <> "+"]
  where
    height = rockHeight r
    drawRow y = "|" <> ((\x -> if Set.member (x, y) parts then '#' else '.') <$> [0 .. 6]) <> "|"

part1 :: IO Int
part1 = do
  jetPattern <- readInputFile "real.txt"
  V.withSizedList jetPattern $ \jetPatternV -> do
    let initialChamber =
          Chamber
            { landedRock = Rock Set.empty,
              jets = mkCycle jetPatternV,
              fallingRocks = mkCycle rocks,
              fallingRockCoord = (2, 3)
            }
        blocks :: Integer
        blocks = 2022
    finalRock <- landedRock <$> go blocks initialChamber
    -- putTextLn $ drawRock finalRock
    pure $ rockHeight finalRock
  where
    go landingsLeft chamber = do
      -- print landingsLeft
      if landingsLeft == 0
        then pure chamber
        else
          let (landed, newChamber) = runState (chamberApplyJet *> chamberApplyGravity) chamber
           in do
                -- print $ fallingRockCoord newChamber
                -- putTextLn . drawRock $ currentFallingRock newChamber
                -- putTextLn . drawRock $ landedRock newChamber
                go (if landed then landingsLeft - 1 else landingsLeft) newChamber

-- -- landRock2 :: Chamber -> Chamber
-- -- landRock2 c@Chamber {landedRock, fallingRockCoord} =
-- --  c
-- --    { landedRock = keepFirstRows 1 newLandedRock,
-- --      fallingRocks = nextFallingRocks c,
-- --      fallingRockCoord = (2, rockHeight newLandedRock + 3)
-- --    }
-- --  where
-- --    newLandedRock =
-- --      joinRocks landedRock
-- --        . rockApplyOffset fallingRockCoord
-- --        $ currentFallingRock c
-- --
-- -- chamberApplyGravity2 :: Chamber -> (Chamber, Bool)
-- -- chamberApplyGravity2 c@Chamber {landedRock, fallingRockCoord} =
-- --  if fallingRockNewY < 0 || overlaps landedRock fallingRockAbsolute
-- --    then (landRock2 c, True)
-- --    else (c {fallingRockCoord = fallingRockNewCoord}, False)
-- --  where
-- --    fallingRockNewCoord@(_, fallingRockNewY) = second (subtract 1) fallingRockCoord
-- --    fallingRockAbsolute = rockApplyOffset fallingRockNewCoord (currentFallingRock c)
-- --
-- -- {-
-- -- Ideas:
-- --- https://en.wikipedia.org/wiki/Cycle_detection
-- --- These algorithms depend on a function applied to a finite set. The chamber grows indefinitely so it is not a finite set. Thus it needs to be culled. Except in pathological cases, this is possible by identifying
-- --- Tortoise and hare algorithm might work
-- ---}
-- -- part2 :: IO Int
-- -- part2 = do
-- --  jetPattern <- readInputFile "sample0.txt"
-- --  let initialChamber =
-- --        Chamber
-- --          { landedRock = Rock Set.empty,
-- --            jets = jetPattern,
-- --            fallingRocks = rocks,
-- --            fallingRockCoord = (2, 3)
-- --          }
-- --  let finalRock = find ((== initialChamber) . snd . traceShowId) $ fmapToSnd (\n -> go jetPattern (n :: Integer) initialChamber) [1 ..]
-- --  print finalRock
-- --  undefined
-- --  where
-- --    go jetPattern landingsLeft chamber =
-- --      -- traceShow landingsLeft $
-- --      -- print landingsLeft
-- --      if landingsLeft == 0
-- --        then chamber
-- --        else
-- --          let (newChamber, landed) = chamberApplyGravity2 $ chamberApplyJet jetPattern chamber
-- --           in do
-- --                -- print $ fallingRockCoord newChamber
-- --                -- putTextLn . drawRock $ landedRock newChamber
-- --                go jetPattern (if landed then landingsLeft - 1 else landingsLeft) newChamber
