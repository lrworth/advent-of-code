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

import Control.Monad.ST (runST)
import Data.Finite (Finite, getFinite, modulo)
import Data.Foldable (Foldable (maximum), minimum)
import Data.STRef (modifySTRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
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

keep :: Int -> Rock -> Rock
keep n r@(Rock parts) =
  Rock $ Set.filter (\(_, y) -> y >= rockHeight r - n) parts

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
grounded (Rock parts) = Rock $ Set.mapMonotonic (\(x, y) -> (x, y - minY)) parts
  where
    minY = minimum $ Set.mapMonotonic snd parts

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
  Rock $ Set.mapMonotonic (\(x, y) -> (x + offsetX, y + offsetY)) parts

rockWidth :: Rock -> Int
rockWidth (Rock parts) = 1 + maximum (Set.mapMonotonic fst parts)

currentFallingRock :: KnownNat nr => Chamber nj nr -> Rock
currentFallingRock Chamber {fallingRocks} = peek fallingRocks

chamberApplyJet :: (KnownNat nj, KnownNat nr) => State (Chamber nj nr) ()
chamberApplyJet = modify' $ \c@Chamber {landedRock, jets, fallingRockCoord} ->
  let (jet, nextJets) = runState pop jets
      fallingRockNewCoord@(fallingRockNewX, _) = first (applyJet jet) fallingRockCoord
      fallingRockAbsolute = rockApplyOffset fallingRockNewCoord $ currentFallingRock c
      fallingRockWidth = rockWidth $ currentFallingRock c
   in if fallingRockNewX < 0 || (fallingRockNewX + fallingRockWidth) > 7 || overlaps landedRock fallingRockAbsolute
        then c {jets = nextJets}
        else c {fallingRockCoord = fallingRockNewCoord, jets = nextJets}

landRock :: (KnownNat nr) => State (Chamber nj nr) ()
landRock = modify' $ \c@Chamber {landedRock, fallingRocks, fallingRockCoord} ->
  let (rock, nextRocks) = runState pop fallingRocks
      newLandedRock =
        joinRocks
          landedRock
          (rockApplyOffset fallingRockCoord rock)
   in c
        { landedRock = newLandedRock,
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

chamberCullPastFullLine :: State (Chamber nj nr) ()
chamberCullPastFullLine = modify' $ \c ->
  c {landedRock = keepTop $ landedRock c}

chamberCullPast :: Int -> State (Chamber nj nr) ()
chamberCullPast n = modify' $ \c ->
  c {landedRock = keep n $ landedRock c}

drawRock :: Rock -> Text
drawRock r@(Rock parts) =
  unlines . fmap toText $ (drawRow <$> [height, height - 1 .. 0]) <> ["+" <> replicate 7 '-' <> "+"]
  where
    height = rockHeight r
    drawRow y = "|" <> ((\x -> if Set.member (x, y) parts then '#' else '.') <$> [0 .. 6]) <> "|"

part1 :: IO Int
part1 = do
  jetPattern <- readInputFile "sample.txt"
  V.withSizedList jetPattern $ \jetPatternV -> do
    let initialChamber =
          Chamber
            { landedRock = Rock Set.empty,
              jets = mkCycle jetPatternV,
              fallingRocks = mkCycle rocks,
              fallingRockCoord = (2, 3)
            }
        blocks :: Int
        blocks = 2022
        finalRock = landedRock $ execState (replicateM blocks chamberLandOneRock) initialChamber
    -- putTextLn $ drawRock finalRock
    pure $ rockHeight finalRock

-- Copied directly from https://en.wikipedia.org/wiki/Cycle_detection#Brent's_algorithm
brent :: (p -> p -> Bool) -> (p -> p) -> p -> (Int, Int)
brent eq f x0 = runST $ do
  -- main phase: search successive powers of two
  power <- newSTRef 1
  lam <- newSTRef 1
  tortoise <- newSTRef x0
  hare <- newSTRef $ f x0 -- f(x0) is the element/node next to x0.
  whileM (liftA2 ((not .) . eq) (readSTRef tortoise) (readSTRef hare)) $ do
    whenM (liftA2 (==) (readSTRef power) (readSTRef lam)) $ do
      -- time to start a new power of two?
      writeSTRef tortoise =<< readSTRef hare
      modifySTRef' power (* 2)
      writeSTRef lam 0
    modifySTRef' hare f
    modifySTRef' lam (+ 1)

  -- Find the position of the first repetition of length λ
  writeSTRef tortoise x0
  writeSTRef hare x0
  lamVal <- readSTRef lam
  replicateM_ lamVal $ do
    modifySTRef' hare f
  -- The distance between the hare and tortoise is now λ.

  -- Next, the hare and tortoise move at same speed until they agree
  mu <- newSTRef 0
  whileM (liftA2 ((not .) . eq) (readSTRef tortoise) (readSTRef hare)) $ do
    modifySTRef' tortoise f
    modifySTRef' hare f
    modifySTRef' mu (+ 1)

  muVal <- readSTRef mu
  pure (lamVal, muVal)

whileM :: Monad m => m Bool -> m () -> m ()
whileM p f =
  p >>= \case
    False -> pass
    True -> f *> whileM p f

until :: Monad m => m Bool -> m ()
until f =
  f >>= \case
    True -> pass
    False -> until f

untilWithCount :: Monad m => m Bool -> m Int
untilWithCount f = go 0
  where
    go n =
      f >>= \case
        True -> pure n
        False -> go (succ n)

chamberLandOneRock :: (KnownNat nj, KnownNat nr) => State (Chamber nj nr) Int
chamberLandOneRock = untilWithCount $ chamberApplyJet *> chamberApplyGravity <* chamberCullPastFullLine

chamberLandOneRockCapping :: (KnownNat nj, KnownNat nr) => Int -> State (Chamber nj nr) Int
chamberLandOneRockCapping maxDropHeight = untilWithCount $ chamberApplyJet *> chamberApplyGravity <* chamberCullPast maxDropHeight

part2 :: IO Int
part2 = do
  jetPattern <- readInputFile "real.txt"
  V.withSizedList jetPattern $ \jetPatternV -> do
    let initialChamber =
          Chamber
            { landedRock = Rock Set.empty,
              jets = mkCycle jetPatternV,
              fallingRocks = mkCycle rocks,
              fallingRockCoord = (2, 3)
            }
        blocks :: Int
        blocks = V.length jetPatternV * V.length rocks
        maxDropHeight = maximum $ evalState (replicateM blocks chamberLandOneRock) initialChamber
        (cycleLength, cycleStart) = brent equalChambers (execState (chamberLandOneRockCapping maxDropHeight)) initialChamber
        preCycle = execState (replicateM cycleStart chamberLandOneRock) initialChamber
        preCycleHeight = rockHeight $ landedRock preCycle
        afterFirstCycle = execState (replicateM cycleLength chamberLandOneRock) preCycle
        afterFirstCycleHeight = rockHeight $ landedRock afterFirstCycle
        cycleHeight = afterFirstCycleHeight - preCycleHeight
        target = 1000000000000
        cycleContribution = target - cycleStart
        (cycles, remainder) = cycleContribution `divMod` cycleLength
        remainderHeight = rockHeight . landedRock $ execState (replicateM remainder chamberLandOneRock) preCycle
    pure $ preCycleHeight + cycles * cycleHeight + (remainderHeight - preCycleHeight)
  where
    equalChambers :: Chamber nj nr -> Chamber nj nr -> Bool
    equalChambers a b =
      (grounded $ landedRock a, jets a, fallingRocks a)
        == (grounded $ landedRock b, jets b, fallingRocks b)
