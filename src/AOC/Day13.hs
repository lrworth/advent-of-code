{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AOC.Day13 where

import Algebra.PrincipalIdealDomain as PID
import Control.Applicative (optional)
import Control.Arrow
import Control.Monad
import qualified Control.Monad.State as State
import Data.Array as A
import qualified Data.Array.ST as STA
import Data.Foldable
import Data.Function
import Data.Graph as Graph
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe as Maybe
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Tree as Tree
import Data.Void (Void)
import Numeric.Natural (Natural)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.RawString.QQ

parse :: String -> (Int, [Maybe Int])
parse =
  ( read
      *** fmap
        ( \case
            "x" -> Nothing
            a -> Just . read $ a
        )
        . splitOn ","
  )
    . (\[a, b] -> (a, b))
    . lines

solveA i =
  let (departureTime, ids) = second catMaybes $ parse i
   in minimumBy (comparing snd) $ fmap (\t -> (t, t - (departureTime `mod` t))) ids

solutionA = uncurry (*) $ solveA input

solveB =
  PID.chineseRemainderMulti
    . catMaybes
    . zipWith (\k -> fmap (,negate k)) [0 ..]
    . snd
    . parse

solutionB = snd . fromJust $ solveB input

sample :: String
sample =
  [r|939
7,13,x,x,59,x,31,19|]

input :: String
input =
  [r|1007125
13,x,x,41,x,x,x,x,x,x,x,x,x,569,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,937,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17|]
