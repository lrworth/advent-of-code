{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Aoc where

import Control.Arrow
import Control.Exception (assert)
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson qualified as Aeson
import Data.Bitraversable
import Data.ByteString.Lazy qualified as ByteString
import Data.Char
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.Graph qualified as Graph
import Data.List (find, head, intercalate, intersperse, isPrefixOf, mapAccumL, sort, sortBy, span, tail, tails, transpose)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Void
import Data.Word ()
import Debug.Trace
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Regex.Applicative
import Text.Show.Pretty
import Prelude hiding (Left, Right, head, tail)

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
  ls <- lines <$> readFile' path
  Just valves <- pure . fmap mconcat $ traverse parseValveLine ls
  pure valves

{- We can use Floyd-Warshall, but what path are we minimising?
    If we say the graph is actually the state space, and edges are the valid state transitions:
    - states are all the locations (being there) and all combinations of valves being turned on and off while we travel. There are far too many of these to store in a table like described in the Wikipedia article, but we might be able to store a sparse map (or even a function (state x state -> value))

  Perhaps iterative deepening (https://en.wikipedia.org/wiki/Iterative_deepening_A*) ?
  - Where the cost estimate only considers the node and the goal.

---
  Two stages:
  1. Calculate travel distance between all nodes so it can be looked-up.
  2. Iterative deepening:
      - From AA to each node. Store the most valuable path with each node.
      - From each node, calculate shortest path to each other node, except
-}
-- shortestPath :: Valve -> Valve -> Set Valve -> Integer
-- shortestPath i j ks =
--  case Set.minView ks of
--    Just (k, kmo) -> min (shortestPath i j kmo) (shortestPath i k kmo + shortestPath k j kmo)
--    Nothing -> cost i j

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

pathValue :: Map Valve ValveData -> DistanceTable -> Int -> [Valve] -> (Int, Int)
pathValue valveMap distanceTable startTime path =
  (endTime, sum segmentValues)
  where
    segments =
      zip
        (Valve "AA" : path)
        path
    (endTime, segmentValues) =
      List.mapAccumL
        ( \timeRemaining (from, to) ->
            let timeRemaining' = timeRemaining - (distance distanceTable from to + 1)
                value = timeRemaining' * flowRate (valveMap Map.! to)
             in (timeRemaining', value)
        )
        startTime
        segments

-- Value of opening all other valves in arbitrary order.
estimateValue :: Map Valve ValveData -> DistanceTable -> [Valve] -> Int
estimateValue valveMap distanceTable path =
  snd $ pathValue valveMap distanceTable timeRemaining remainingValves
  where
    remainingValves = toList $ Set.difference (Map.keysSet valveMap) (Set.fromList path)
    (timeRemaining, _) = pathValue valveMap distanceTable 30 path

-- data SearchResult = Found [[Valve]] | Infinity | Threshold Int [[Valve]]
--
-- data IDAStarResult = ISRFound ([[Valve]], Int) | ISRNotFound
--
-- iterativeDeepeningAStar :: Map Valve ValveData -> DistanceTable -> IDAStarResult
-- iterativeDeepeningAStar valveMap distanceTable =
--  go (estimateValue valveMap distanceTable root) [root]
--  where
--    root = []
--    go bound path =
--      case search valveMap distanceTable path 0 bound of
--        Found newPath -> ISRFound (newPath, bound)
--        Infinity -> ISRNotFound
--        Threshold t newPath -> go t newPath
--
-- search :: Map Valve ValveData -> DistanceTable -> [[Valve]] -> Int -> Int -> SearchResult
-- search valveMap distanceTable path g bound =
--  let node = head path
--      f = g + estimateValue valveMap distanceTable node
--      valves = Map.keys valveMap
--      remainingValves = toList $ Set.difference (Map.keysSet valveMap) (Set.fromList node)
--   in if
--          | f > bound -> Threshold f path
--          | length node == length valves -> Found path
--          | otherwise ->
--              let successors = fmap (\v -> path ++ [v]) remainingValves
--               in go Nothing successors
--  where
--    go mMin (s : succs) =
--      case search valveMap distanceTable (s : path) (pathValue valueMap distanceTable 30 s) bound of
--        Found newPath -> Found newPath
--        Infinity -> go mMin succs
--        Threshold m' newPaths -> case mMin of
--          Nothing -> go (Just m') succs
--          Just m -> if m' <= m then go (Just m') succs else go (Just m) succs

bestPath :: Map Valve ValveData -> DistanceTable -> [Valve]
bestPath valveMap distanceTable = go []
  where
    candidateValves = Set.fromList . filter ((/= 0) . flowRate . (valveMap Map.!)) $ Map.keys valveMap
    go pathSoFar =
      case Set.minView $ Set.difference candidateValves (Set.fromList pathSoFar) of
        Nothing -> pathSoFar
        Just (addValve, _) ->
          go $
            (\x -> traceShow ("result" ++ show (pathValue valveMap distanceTable 30 x)) x) $
              maximumBy (comparing (\x -> (\v -> traceShow (x, v) v) . snd . pathValue valveMap distanceTable 30 $ x)) $
                traceShowId $
                  insertions addValve pathSoFar

insertions :: a -> [a] -> [[a]]
insertions z [] = [[z]]
insertions z (x : xs) = (z : x : xs) : ((x :) <$> insertions z xs)

part1 :: IO Integer
part1 = do
  valveMap <- readValveFile "real.txt"
  let distanceTable = mkDistanceTable valveMap
  let possiblePaths = List.permutations . filter ((/= 0) . flowRate . (valveMap Map.!)) $ Map.keys valveMap
  print $ length possiblePaths
  -- print . (\x -> (x, pathValue valveMap distanceTable 30 x)) $ bestPath valveMap distanceTable
  -- print $
  --  pathValue
  --    valveMap
  --    distanceTable
  --    30
  --    [ Valve "DD",
  --      Valve "BB",
  --      Valve "JJ",
  --      Valve "HH",
  --      Valve "EE",
  --      Valve "CC"
  --    ]

  -- print $ length possiblePaths
  -- print $ maximum $ fmap (snd . pathValue valveMap distanceTable 30) possiblePaths
  -- print $ pathValue valveMap distanceTable 30 [Valve "EI", Valve "OA"]
  -- print $ estimateValue valveMap distanceTable [Valve "EI", Valve "OA"]
  undefined
