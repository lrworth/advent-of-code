{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, isPrefixOf, mapAccumL, sort, span, tails, transpose)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import GHC.Generics hiding (to)
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty
import Prelude hiding (Left, Right, head, tail)

newtype WorryLevel = WorryLevel Integer
  deriving (Show)

newtype MonkeyId = MonkeyId {unMonkeyId :: Int}
  deriving (Enum, Eq, Ord, Show)

data Monkey = Monkey
  { items :: [WorryLevel],
    operation :: WorryLevel -> WorryLevel,
    divisibleBy :: Integer,
    throwTo :: Bool -> MonkeyId
  }

worryLevelTest :: Integer -> WorryLevel -> Bool
worryLevelTest divisibleBy (WorryLevel level) = (level `mod` divisibleBy) == 0

parseMonkey :: [String] -> Maybe (MonkeyId, Monkey)
parseMonkey ls = do
  [lId, lItems, lOperation, lTest, lTrue, lFalse] <- pure ls
  [sId] <- pure . drop 1 . words $ init lId
  monkeyId <- MonkeyId <$> readMaybe sId
  items <- traverse (fmap WorryLevel . readMaybe) . splitOn "," . concat . drop 2 . words $ lItems
  [sOperator, sOperand] <- pure . drop 4 $ words lOperation
  Just operator <- pure $ case sOperator of
    "*" -> Just (*)
    "+" -> Just (+)
    _ -> Nothing
  operation <- case sOperand of
    "old" -> Just $ \(WorryLevel old) -> WorryLevel $ operator old old
    _ -> (\operand -> \(WorryLevel old) -> WorryLevel $ operator old operand) <$> readMaybe sOperand
  [sDivisibleBy] <- pure . drop 3 $ words lTest
  divisibleBy <- readMaybe sDivisibleBy
  [sTrue] <- pure . drop 5 $ words lTrue
  [sFalse] <- pure . drop 5 $ words lFalse
  true <- readMaybe sTrue
  false <- readMaybe sFalse
  let throwTo = \case
        True -> MonkeyId true
        False -> MonkeyId false
  pure (monkeyId, Monkey {..})

newtype Monkeys = Monkeys (Map MonkeyId Monkey)

readMonkeyFile :: String -> IO Monkeys
readMonkeyFile path = do
  ls <- lines <$> readFile' path
  Just monkeys <- pure . fmap (Monkeys . Map.fromList) . traverse parseMonkey $ splitOn [""] ls
  pure monkeys

reduceWorryLevel :: WorryLevel -> WorryLevel
reduceWorryLevel (WorryLevel l) = WorryLevel $ l `div` 3

monkeyTurn :: (MonadState (Map MonkeyId Monkey) m, MonadFail m, MonadWriter [MonkeyId] m) => MonkeyId -> m ()
monkeyTurn monkeyId = do
  Just currentMonkey <- gets $ Map.lookup monkeyId
  let inspectItem = throwTo currentMonkey . worryLevelTest (divisibleBy currentMonkey)
  let throwItem wl targetMonkeyId = modify $ Map.adjust (\m -> m {items = items m ++ [wl]}) targetMonkeyId
  traverse_
    ( \worryLevel -> do
        let newWorryLevel = reduceWorryLevel . operation currentMonkey $ worryLevel
        throwItem newWorryLevel $ inspectItem newWorryLevel
        tell [monkeyId]
    )
    (items currentMonkey)
  modify $ Map.adjust (\m -> m {items = []}) monkeyId

monkeyRound :: (MonadState (Map MonkeyId Monkey) m, MonadFail m, MonadWriter [MonkeyId] m) => m ()
monkeyRound = do
  numMonkeys <- gets Map.size
  traverse_ monkeyTurn [MonkeyId 0 .. MonkeyId (numMonkeys - 1)]

part1 :: IO Int
part1 = do
  Monkeys monkeyMap <- readMonkeyFile "real.txt"
  monkeyIds <- execWriterT $ runStateT (replicateM 20 monkeyRound) monkeyMap
  let monkeyInspections = Map.fromListWith (+) $ zip monkeyIds $ repeat 1
  let monkeyBusiness = product . take 2 . reverse . sort $ Map.elems monkeyInspections
  pure monkeyBusiness

readMonkeyFile2 :: String -> IO (IntMap Monkey)
readMonkeyFile2 path = do
  ls <- lines <$> readFile' path
  Just monkeys <- pure . fmap (IntMap.fromList) . traverse (fmap (first unMonkeyId) . parseMonkey) $ splitOn [""] ls
  pure monkeys

monkeyTurn2 :: (MonadState (IntMap Monkey) m, MonadFail m, MonadWriter [Int] m, MonadReader (WorryLevel -> WorryLevel) m) => Int -> m ()
monkeyTurn2 monkeyId = do
  Just currentMonkey <- gets $ IntMap.lookup monkeyId
  let inspectItem = throwTo currentMonkey . worryLevelTest (divisibleBy currentMonkey)
  let throwItem wl targetMonkeyId = modify' $ IntMap.adjust (\m -> m {items = items m ++ [wl]}) targetMonkeyId
  reduceWorryLevel' <- ask
  traverse_
    ( \worryLevel -> do
        let newWorryLevel = reduceWorryLevel' $ operation currentMonkey worryLevel
        throwItem newWorryLevel $ unMonkeyId $ inspectItem newWorryLevel
        tell [monkeyId]
    )
    (items currentMonkey)
  modify' $ IntMap.adjust (\m -> m {items = []}) monkeyId

monkeyRound2 :: (MonadState (IntMap Monkey) m, MonadFail m, MonadWriter [Int] m, MonadReader (WorryLevel -> WorryLevel) m) => m ()
monkeyRound2 = do
  numMonkeys <- gets IntMap.size
  traverse_ monkeyTurn2 [0 .. (numMonkeys - 1)]

part2 :: IO Integer
part2 = do
  monkeyMap <- readMonkeyFile2 "real.txt"
  let worryLevelModulus = product $ divisibleBy <$> IntMap.elems monkeyMap
  let reduceWorryLevel' = \(WorryLevel level) -> WorryLevel $ level `mod` worryLevelModulus
  monkeyIds <- execWriterT $ runReaderT (runStateT (replicateM 10000 monkeyRound2) monkeyMap) reduceWorryLevel'
  let monkeyInspections = Map.fromListWith (+) $ zip monkeyIds $ repeat 1
  print monkeyInspections
  let monkeyBusiness = product . take 2 . reverse . sort $ Map.elems monkeyInspections
  pure monkeyBusiness

main :: IO ()
main = part2 >>= print
