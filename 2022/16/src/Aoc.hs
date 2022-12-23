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
import Data.List (find, intercalate, intersperse, isPrefixOf, mapAccumL, sort, sortBy, span, tail, tails, transpose)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
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

distance :: Map Valve ValveData -> Valve -> Valve -> State S Int
distance valveMap from to = do
  dM <- gets $ Map.lookup (from, to) . distanceMemo
  case dM of
    Just d -> pure d
    Nothing -> do
      let d = bfs 0 [from]
      modify $ \s -> s {distanceMemo = Map.insert (from, to) d $ distanceMemo s}
      pure d
  where
    bfs d vs =
      if to `elem` vs
        then d
        else
          let newVs = do
                v <- vs
                let ValveData {tunnels} = valveMap Map.! v
                tunnels
           in bfs (succ d) newVs

totalFlowRate :: Map Valve ValveData -> Set Valve -> Int
totalFlowRate valveMap =
  sum
    . fmap (\v -> flowRate $ valveMap Map.! v)
    . toList

annotate :: Show a => String -> a -> a
annotate s a = traceShow (s <> ": " <> show a) a

memoPure :: Valve -> Set Valve -> Int -> State S Int
memoPure v goalVs n = do
  modify $ \s -> s {costMemo = Map.insert (v, goalVs) n $ costMemo s}
  -- traceShowM =<< gets (Map.size . costMemo)
  pure n

data S = S
  { costMemo :: Map (Valve, Set Valve) Int,
    distanceMemo :: Map (Valve, Valve) Int
  }

cost :: Map Valve ValveData -> Valve -> Set Valve -> Set Valve -> State S Int
cost valveMap v goalVs avoid = do
  -- First see if it's been visited before
  prior <- gets $ Map.lookup (v, goalVs) . costMemo
  case prior of
    Just i -> pure i
    Nothing ->
      memoPure v goalVs =<< do
        let thisFlowRate = flowRate $ valveMap Map.! v
        if Set.null goalVs
          then pure thisFlowRate
          else do
            -- Cost of opening the current valve then traversing everything
            costOpenCurrent <-
              traverse
                ( \goalV -> do
                    let remaining = Set.delete goalV goalVs
                    c <-
                      cost
                        valveMap
                        goalV
                        remaining
                        avoid
                    d <- distance valveMap v goalV
                    pure
                      ( thisFlowRate
                          + ((1 + d) * totalFlowRate valveMap goalVs)
                          + c
                      )
                )
                (toList goalVs)
            -- Cost of skipping the current valve and traversing from somewhere else
            costSkipCurrent <-
              traverse
                ( \goalV ->
                    if Set.member goalV avoid
                      then pure Nothing
                      else do
                        let remaining = Set.delete goalV $ Set.insert v goalVs
                        c <-
                          cost
                            valveMap
                            goalV
                            remaining
                            (Set.insert v avoid)
                        d <- distance valveMap v goalV
                        pure $
                          Just
                            ( d * totalFlowRate valveMap (Set.insert v goalVs)
                                + c
                            )
                )
                (toList goalVs)

            pure $
              minimum $
                costOpenCurrent
                  ++ catMaybes costSkipCurrent

part1 :: IO Int
part1 = do
  valveMap <- readValveFile "real.txt"
  let zeroFlowValves = Map.keysSet $ Map.filter ((== 0) . flowRate) valveMap
  let absoluteFlowRate = 30 * sum (flowRate <$> Map.elems valveMap)
  let absoluteCost = evalState (cost valveMap (Valve "AA") ((`Set.difference` zeroFlowValves) . Set.delete (Valve "AA") $ Map.keysSet valveMap) Set.empty) S {costMemo = Map.empty, distanceMemo = Map.empty}
  traceShowM absoluteFlowRate
  traceShowM absoluteCost

  pure $ absoluteFlowRate - absoluteCost
