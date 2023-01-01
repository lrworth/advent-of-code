{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Aoc where

import Data.Char
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Word ()
-- import Relude.Extra.Tuple (mapToSnd)

import Relude.Extra.Tuple (toSnd)
import Relude.Unsafe (read)
import Text.Regex.Applicative
import Witherable ((<$?>))

newtype Valve = Valve String
  deriving (Eq, Ord, Show)

data ValveData = ValveData
  { flowRate :: Int,
    tunnels :: [Valve]
  }
  deriving (Show)

-- Blog idea: Use of regex-applicative
optPlural :: RE Char String -> RE Char String
optPlural = (<* optional "s")

sepBy :: RE s a -> RE s b -> RE s [a]
sepBy ra rs = many (ra <* optional rs)

valveRE :: RE Char Valve
valveRE = Valve <$> many (psym isLetter)

parseValveLine :: String -> Maybe (Map Valve ValveData)
parseValveLine = match $ do
  valveID <- "Valve " *> valveRE
  flowRate <- " has flow rate=" *> (read <$> many (psym isDigit))
  tunnels <-
    optPlural "; tunnel"
      *> optPlural " lead"
      *> optPlural " to valve"
      *> " "
      *> (valveRE `sepBy` ", ")
  pure $ Map.singleton valveID ValveData {..}

readValveFile :: String -> IO (Map Valve ValveData)
readValveFile path = do
  ls <- lines . decodeUtf8 <$> readFileBS path
  Just valves <- pure $ mconcat <$> traverse (parseValveLine . toString) ls
  pure valves

edges :: Map Valve ValveData -> [(Valve, Valve)]
edges =
  concatMap (\(v, ValveData {tunnels}) -> fmap (v,) tunnels)
    . Map.assocs

newtype DistanceTable = DistanceTable (Map (Valve, Valve) Int)
  deriving (Show)

-- Floyd-Warshall algorithm.
mkDistanceTable :: Map Valve ValveData -> DistanceTable
mkDistanceTable valveMap = DistanceTable . (`execState` Map.empty) $ do
  forM_ (edges valveMap) $ \(u, v) ->
    modify' $ Map.insert (u, v) 1
  forM_ valves $ \v ->
    modify' $ Map.insert (v, v) 0
  forM_ valves $ \k ->
    forM_ valves $ \i ->
      forM_ valves $ \j ->
        modify' $ \dt ->
          let ij = distLookup (i, j) dt
              ik = distLookup (i, k) dt
              kj = distLookup (k, j) dt
              ij' = ik + kj
           in if ij' < ij then Map.insert (i, j) ij' dt else dt
  where
    valves = Map.keys valveMap
    numValves = length valves
    distLookup k dt = fromMaybe numValves $ Map.lookup k dt

distance :: DistanceTable -> Valve -> Valve -> Int
distance (DistanceTable dt) from to =
  fromMaybe (error "unknown valves") $ Map.lookup (from, to) dt

unfoldForest' :: (b -> [b]) -> [b] -> [Tree b]
unfoldForest' = Tree.unfoldForest . toSnd

data MazeState = MazeState
  { location :: Valve,
    openValves :: Set Valve,
    timeRemaining :: Int
  }
  deriving (Show)

nextStates :: Set Valve -> DistanceTable -> MazeState -> [MazeState]
nextStates usefulValves distanceTable MazeState {location, openValves, timeRemaining} =
  visitValve <$?> remainingValves
  where
    remainingValves = toList $ Set.difference usefulValves openValves
    visitValve v =
      let newTimeRemaining = timeRemaining - (distance distanceTable location v + 1)
       in if newTimeRemaining > 0
            then
              Just
                MazeState
                  { location = v,
                    openValves = Set.insert v openValves,
                    timeRemaining = newTimeRemaining
                  }
            else Nothing

pathWithValueFold :: Map Valve ValveData -> MazeState -> [[([Valve], Int)]] -> [([Valve], Int)]
pathWithValueFold valveMap MazeState {location, timeRemaining} tailsWithValue =
  if null tailsWithValue
    then [addCurrent ([], 0)]
    else addCurrent <$> join tailsWithValue
  where
    addCurrent :: ([Valve], Int) -> ([Valve], Int)
    addCurrent (pathTail, tailValue) =
      ( location : pathTail,
        tailValue + timeRemaining * flowRate (valveMap Map.! location)
      )

part1 :: IO ([Valve], Int)
part1 = do
  valveMap <- readValveFile "real.txt"
  let usefulValves = Map.keysSet $ Map.filter (\ValveData {flowRate} -> flowRate > 0) valveMap
  let distanceTable = mkDistanceTable valveMap
  let initialState = MazeState {location = Valve "AA", openValves = Set.empty, timeRemaining = 30}
  let stateForest = unfoldForest' (nextStates usefulValves distanceTable) [initialState]
  let pathsWithValue = Tree.foldTree (pathWithValueFold valveMap) =<< stateForest
  pure $ List.maximumBy (comparing snd) pathsWithValue

partitions :: (Ord a) => Set a -> Set (Set a, Set a)
partitions as = Set.map (toSnd (as Set.\\)) $ Set.powerSet as

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (a, b) = if a < b then (a, b) else (b, a)

-- Solution prompted by https://www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j0fti6c/?utm_source=share&utm_medium=web2x&context=3
part2 :: IO ([Valve], [Valve], Int)
part2 = do
  valveMap <- readValveFile "real.txt"
  let usefulValves = Map.keysSet $ Map.filter (\ValveData {flowRate} -> flowRate > 0) valveMap
  let splitUsefulValves = Set.map sortPair $ partitions usefulValves
  print $ length splitUsefulValves
  let distanceTable = mkDistanceTable valveMap
  pure $ List.maximumBy (comparing (\(_, _, a) -> a)) $ Set.map (solve valveMap distanceTable) splitUsefulValves
  where
    solve valveMap distanceTable (meValves, elephantValves) =
      let initialState = MazeState {location = Valve "AA", openValves = Set.empty, timeRemaining = 26}
          meStateForest = unfoldForest' (nextStates meValves distanceTable) [initialState]
          mePathsWithValue = Tree.foldTree (pathWithValueFold valveMap) =<< meStateForest
          elephantStateForest = unfoldForest' (nextStates elephantValves distanceTable) [initialState]
          elephantPathsWithValue = Tree.foldTree (pathWithValueFold valveMap) =<< elephantStateForest
          (meBestPath, meValue) = List.maximumBy (comparing snd) mePathsWithValue
          (elephantBestPath, elephantValue) = List.maximumBy (comparing snd) elephantPathsWithValue
       in (meBestPath, elephantBestPath, meValue + elephantValue)
