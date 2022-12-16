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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Data
import Data.Foldable
import Data.Generics.Labels ()
import Data.List (find, isPrefixOf, mapAccumL, sort, sortBy, span, tails, transpose)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word ()
import Debug.Trace
import GHC.Generics hiding (to)
import Numeric.Natural (Natural)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty
import Prelude hiding (Left, Right, head, tail)

data Packet = PList [Packet] | PInt Int
  deriving (Eq, Show)

comparePackets :: Packet -> Packet -> Ordering
comparePackets (PList ps) (PList qs) =
  case find (/= EQ) $ zipWith comparePackets ps qs of
    Just c -> c
    Nothing -> compare (length ps) (length qs)
comparePackets (PList ps) (PInt n) = comparePackets (PList ps) (PList [PInt n])
comparePackets (PInt n) (PList ps) = comparePackets (PList [PInt n]) (PList ps)
comparePackets (PInt n) (PInt m) = compare n m

instance Aeson.FromJSON Packet where
  parseJSON = \case
    Aeson.Array a -> PList <$> (traverse Aeson.parseJSON $ toList a)
    Aeson.Number n -> pure . PInt $ round n
    _ -> error "wrong"

readPacketPairsFile :: String -> IO [(Packet, Packet)]
readPacketPairsFile path = do
  packetPairsRaw <- chunksOf 3 . ByteString.split 10 <$> ByteString.readFile path
  let readPacketPair raw = do
        [a, b] <- pure $ take 2 raw
        [a', b'] <- traverse Aeson.decode' [a, b]
        pure (a', b')
  Just x <- pure $ traverse readPacketPair packetPairsRaw
  pure x

part1 :: IO Int
part1 = do
  packetPairs <- readPacketPairsFile "real.txt"
  pure $
    sum
      . fmap fst
      . filter ((/= GT) . snd)
      . zip [1 ..]
      $ uncurry comparePackets <$> packetPairs

readPacketsFile :: String -> IO [Packet]
readPacketsFile path = do
  packetsRaw <- filter (not . ByteString.null) . ByteString.split 10 <$> ByteString.readFile path
  Just x <- pure $ traverse Aeson.decode' packetsRaw
  pure x

part2 :: IO Int
part2 = do
  let two = PList [PList [PInt 2]]
  let six = PList [PList [PInt 6]]
  packets <- (two :) . (six :) <$> readPacketsFile "real.txt"
  pure . product . fmap fst . filter ((`elem` [two, six]) . snd) . zip [1 ..] $ sortBy comparePackets packets
