{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Control.Arrow
import Control.Exception (assert)
import Control.Lens
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Aeson as Aeson
import Data.Bitraversable
import qualified Data.ByteString.Lazy as ByteString
import Data.Char
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import qualified Data.Graph as Graph
import Data.List (find, intercalate, intersperse, isPrefixOf, mapAccumL, sort, sortBy, span, tail, tails, transpose)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Void
import Data.Word ()
import Debug.Trace
import GHC.Generics hiding (to)
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

{- This is too slow.

data Action = Move Valve | Open
  deriving (Show, Ord, Eq)

data Step = Step
  { action :: Action,
    -- After the action has been performed, what valves are open?
    openValves :: Set Valve,
    nextSteps :: [Step]
  }
  deriving (Show)

solutionSpace :: Map Valve ValveData -> Valve -> Set Valve -> [Step]
solutionSpace valves valve openValves =
  let ValveData {tunnels, flowRate} = valves Map.! valve
   in ( \action ->
          let newOpenValves = case action of
                Move _ -> openValves
                Open -> Set.insert valve openValves
              nextValve = case action of
                Move v -> v
                Open -> valve
              nextSteps = solutionSpace valves nextValve newOpenValves
           in Step
                { action,
                  openValves = newOpenValves,
                  nextSteps
                }
      )
        <$> ( (if Set.member valve openValves || flowRate == 0 then [] else [Open])
                ++ (Move <$> tunnels)
            )

pruneAfterDepth :: Int -> Step -> Step
pruneAfterDepth n s@Step {nextSteps} =
  s
    { nextSteps =
        if n <= 1
          then []
          else fmap (pruneAfterDepth (n - 1)) nextSteps
    }

pruneUselessMoves :: Set Valve -> Step -> Step
pruneUselessMoves contiguousMoves s@Step {action, nextSteps} =
  s
    { nextSteps =
        case action of
          Open -> pruneUselessMoves Set.empty <$> nextSteps
          Move v ->
            if Set.member v contiguousMoves
              then []
              else pruneUselessMoves (Set.insert v contiguousMoves) <$> nextSteps
    }

pruneTrailingMoves :: Step -> Maybe Step
pruneTrailingMoves s@Step {action, nextSteps} =
  case action of
    Move _ ->
      if List.null nextSteps
        then Nothing
        else
          let ns = catMaybes $ pruneTrailingMoves <$> nextSteps
           in if List.null ns then Nothing else Just s {nextSteps = ns}
    Open ->
      Just $
        s {nextSteps = catMaybes $ pruneTrailingMoves <$> nextSteps}

allBranches :: Step -> [[Action]]
allBranches Step {action, nextSteps} = do
  if List.null nextSteps
    then [[action]]
    else do
      rest <- allBranches <$> nextSteps
      (action :) <$> rest

evaluatePath :: Int -> Map Valve ValveData -> Valve -> [Action] -> Int
evaluatePath _ _ _ [] = 0
evaluatePath timeRemaining valves valve (action : path) =
  let ValveData {flowRate} = valves Map.! valve
   in if timeRemaining == 0
        then 0
        else case action of
          Open -> flowRate * (timeRemaining - 1) + evaluatePath (timeRemaining - 1) valves valve path
          Move v -> evaluatePath (timeRemaining - 1) valves v path

part1 :: IO Int
part1 = do
  valveMap <- readValveFile "real.txt"
  print
    . fmap (\a -> (evaluatePath 30 valveMap (Valve "AA") a, a))
    . concat
    . fmap allBranches
    . catMaybes
    $ pruneTrailingMoves . pruneUselessMoves Set.empty . pruneAfterDepth 6 <$> solutionSpace valveMap (Valve "AA") Set.empty
  undefined
-}
