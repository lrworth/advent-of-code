{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Control.Exception (throwIO)
import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.Semigroup (Max (..))
import Data.Text qualified as Text
import Generic.Data (Generically (..), Generically1 (..))
import Generic.Data.Orphans ()
import Path (File, Path, Rel, relfile, toFilePath)
import Relude.Unsafe (fromJust)
import Text.Megaparsec (MonadParsec, Parsec, anySingle, choice, chunk, parse, parseTest, sepBy, single, try)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Stream (Token)

input :: Path Rel File -> IO [Text]
input = fmap (lines . decodeUtf8) . readFileBS . toFilePath

parseIO :: Parsec Void Text a -> Path r File -> Text -> IO a
parseIO parser path stream = either throwIO pure $ parse parser (toFilePath path) stream

inputLines :: Parsec Void Text a -> Path Rel File -> IO [a]
inputLines parser path = traverse (parseIO parser path) =<< input path

data Game = Game {gameNumber :: Int, plays :: [Play]}
  deriving stock (Show)

type Play = Play' Sum

type PlayMax = Play' Max

data Play' f = Play {red :: f Int, green :: f Int, blue :: f Int}
  deriving stock (Generic)

deriving via Generically Play instance Semigroup Play

deriving via Generically Play instance Monoid Play

deriving via Generically Play instance Show Play

deriving via Generically PlayMax instance Semigroup PlayMax

deriving via Generically PlayMax instance Monoid PlayMax

deriving via Generically PlayMax instance Show PlayMax

pGame :: Parsec Void Text Game
pGame = do
  gameNumber <- chunk "Game " *> L.decimal <* ": "
  plays <- sepBy pPlay (chunk "; ")
  pure Game {..}

pPlay :: Parsec Void Text Play
pPlay =
  mconcat <$> sepBy pColour (chunk ", ")

pColour :: Parsec Void Text Play
pColour = do
  numCubes <- Sum <$> (L.decimal <* space1)
  choice
    [ (chunk "red" $> (mempty {red = numCubes})),
      (chunk "green" $> (mempty {green = numCubes})),
      (chunk "blue" $> (mempty {blue = numCubes}))
    ]

solveOne :: [Game] -> Int
solveOne games = sum $ gameNumber <$> filter isGamePossible games

isGamePossible :: Game -> Bool
isGamePossible Game {plays} = all isPlayPossible plays

isPlayPossible :: Play -> Bool
isPlayPossible Play {red, green, blue} =
  red <= 12 && green <= 13 && blue <= 14

solveTwo :: [Game] -> Int
solveTwo = sum . fmap (playPower . minCubesRequired)

playPower :: (Coercible (f Int) Int) => Play' f -> Int
playPower Play {red, green, blue} = coerce red * coerce green * coerce blue

minCubesRequired :: Game -> PlayMax
minCubesRequired (Game {plays}) = foldMap (\Play {..} -> Play {red = coerce red, green = coerce green, blue = coerce blue}) plays
