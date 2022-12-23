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
import Data.Map (Map)
import Data.Map qualified as Map
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

distance :: Map Valve ValveData -> Valve -> Valve -> Int
distance valveMap from to = bfs 0 [from]
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

cost :: Map Valve ValveData -> Valve -> Set Valve -> Set Valve -> Int
cost valveMap v goalVs = \avoid ->
  let thisFlowRate = flowRate $ valveMap Map.! v
   in -- annotate (show v <> ", " <> show goalVs) $
      if Set.null goalVs
        then thisFlowRate
        else
          minimum . fmap snd $
            -- Cost of opening the current valve then traversing everything
            toList
              ( Set.map
                  ( \goalV ->
                      let remaining = Set.delete goalV goalVs
                       in ( goalV,
                            thisFlowRate
                              + ((1 + distance valveMap v goalV) * totalFlowRate valveMap goalVs)
                              + cost
                                valveMap
                                goalV
                                remaining
                                avoid
                          )
                  )
                  goalVs
              )
              -- Cost of skipping the current valve and traversing from somewhere else
              ++ catMaybes
                ( toList
                    ( Set.map
                        ( \goalV ->
                            if Set.member goalV avoid
                              then Nothing
                              else
                                Just $
                                  let remaining = Set.delete goalV $ Set.insert v goalVs
                                   in ( goalV,
                                        distance valveMap v goalV * totalFlowRate valveMap (Set.insert v goalVs)
                                          + cost
                                            valveMap
                                            goalV
                                            remaining
                                            (Set.insert v avoid)
                                      )
                        )
                        goalVs
                    )
                )

part1 :: IO Int
part1 = do
  valveMap <- readValveFile "sample.txt"
  let absoluteFlowRate = 30 * sum (flowRate <$> Map.elems valveMap)
  pure $ absoluteFlowRate - cost valveMap (Valve "AA") (Set.delete (Valve "AA") $ Map.keysSet valveMap) Set.empty
