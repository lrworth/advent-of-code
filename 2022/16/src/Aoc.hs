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

data MazeState2 = MazeState2
  { location1 :: Valve,
    location2 :: Valve,
    avoid1 :: !(Set Valve),
    avoid2 :: !(Set Valve),
    newlyOpen :: !(Set Valve),
    openValves :: !(Set Valve),
    timeRemaining :: !Int
  }
  deriving (Show)

nextStates2 :: Map Valve ValveData -> MazeState2 -> [MazeState2]
nextStates2 valveMap MazeState2 {location1, location2, avoid1, avoid2, openValves, timeRemaining} = do
  let nextActions1 = nextActions avoid1 location1
  let nextActions2 = nextActions avoid2 location2
  a1 <- nextActions1
  a2 <- nextActions2
  let newNewlyOpen =
        ( case a1 of
            Nothing -> Set.insert location1
            Just _ -> id
        )
          . ( case a2 of
                Nothing -> Set.insert location2
                Just _ -> id
            )
          $ Set.empty
      newLocation1 = fromMaybe location1 a1
      newLocation2 = fromMaybe location2 a2
      newTimeRemaining = timeRemaining - 1
      newAvoid1 = case a1 of
        Nothing -> Set.empty
        Just _ -> Set.insert location1 avoid1
      newAvoid2 = case a2 of
        Nothing -> Set.empty
        Just _ -> Set.insert location2 avoid2
  if newTimeRemaining > 0
    then
      pure
        MazeState2
          { location1 = newLocation1,
            location2 = newLocation2,
            avoid1 = newAvoid1,
            avoid2 = newAvoid2,
            newlyOpen = newNewlyOpen,
            openValves = openValves `Set.union` newNewlyOpen,
            timeRemaining = newTimeRemaining
          }
    else []
  where
    -- Nothing means they opened the valve
    nextActions :: Set Valve -> Valve -> [Maybe Valve]
    nextActions avoid valve =
      ( Just
          <$> toList
            ( Set.fromList (tunnels (valveMap Map.! valve))
                Set.\\ avoid
            )
      )
        <> [Nothing | flowRate (valveMap Map.! valve) > 0 && Set.notMember valve openValves]

pathWithValueFold2 :: Map Valve ValveData -> MazeState2 -> [[(Int)]] -> [(Int)]
pathWithValueFold2 valveMap MazeState2 {location1, location2, newlyOpen, timeRemaining} tailsWithValue =
  if null tailsWithValue
    then [addCurrent (0)]
    else addCurrent <$> join tailsWithValue
  where
    addCurrent :: (Int) -> (Int)
    addCurrent (tailValue) =
      ( tailValue + timeRemaining * sum (flowRate . (valveMap Map.!) <$> toList newlyOpen)
      )

part2 :: IO Int
part2 = do
  valveMap <- readValveFile "real.txt"
  let initialState =
        MazeState2
          { location1 = Valve "AA",
            location2 = Valve "AA",
            avoid1 = Set.empty,
            avoid2 = Set.empty,
            newlyOpen = Set.empty,
            openValves = Set.empty,
            timeRemaining = 26
          }
  let stateForest = unfoldForest' (nextStates2 valveMap) [initialState]
  -- print $ length . Tree.flatten <$> stateForest
  -- putStrLn $ Tree.drawForest $ show <<$>> stateForest
  let pathsWithValue = Tree.foldTree (pathWithValueFold2 valveMap) =<< stateForest
  -- print $ (\(a, _, _) -> length a) <$> pathsWithValue
  pure $ List.maximum pathsWithValue
