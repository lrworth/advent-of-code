{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module AOC.Day10 where

import Control.Applicative (optional)
import Control.Arrow
import Control.Monad
import Control.Monad.State as State
import Data.Array as A
import qualified Data.Array.ST as STA
import Data.Foldable
import Data.Function
import Data.Graph as Graph
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Tree as Tree
import Data.Void (Void)
import Numeric.Natural (Natural)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.RawString.QQ

solveA :: String -> Int
solveA i =
  let sorted =
        (0 :)
          . L.sort
          . fmap read
          . lines
          $ i
      builtIn = last sorted + 3
      sorted' = sorted ++ [builtIn]
   in differences 1 sorted' * differences 3 sorted'

differences :: Int -> [Int] -> Int
differences n xs =
  length $ filter (== True) $ zipWith (\x y -> y == x + n) xs (tail xs)

solveB :: String -> Int
solveB i =
  let sorted' =
        (0 :)
          . L.sort
          . fmap read
          . lines
          $ i
      builtIn = last sorted' + 3
      sorted = sorted' ++ [builtIn]
   in validArrangements sorted

-- Too slow
--validArrangements :: [Int] -> [[Int]]
--validArrangements p = case p of
--  [] -> []
--  (x : xs) ->
--    let validTails =
--          takeWhile (\case (a : _) -> a - x <= 3; [] -> False) $ L.tails xs
--     in if L.null validTails
--          then [[x]]
--          else fmap (x :) . validArrangements =<< validTails

validArrangements :: [Int] -> Int
validArrangements xs' =
  let xs = L.sort xs'
      (l, h) = (minimum &&& maximum) xs
      pathsToXs = STA.runSTArray do
        a <- STA.newArray (l, h) Nothing
        STA.writeArray a l $ Just 1
        tail xs & traverse_ \x -> do
          pathsToX <-
            fmap (sum . catMaybes)
              . traverse (STA.readArray a)
              . filter (>= l)
              $ fmap (x -) [1 .. 3]
          STA.writeArray a x $ Just (pathsToX :: Int)
        pure a
   in maybe undefined id $ (pathsToXs ! h :: Maybe Int)

sample1 :: String
sample1 =
  [r|16
10
15
5
1
11
7
19
6
12
4|]

sample2 :: String
sample2 =
  [r|28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3|]

input :: String
input =
  [r|97
62
23
32
51
19
98
26
90
134
73
151
116
76
6
94
113
127
119
44
115
50
143
150
86
91
36
104
131
101
38
66
46
96
54
70
8
30
1
108
69
139
24
29
77
124
107
14
137
16
140
80
68
25
31
59
45
126
148
67
13
125
53
57
41
47
35
145
120
12
37
5
110
138
130
2
63
83
22
79
52
7
95
58
149
123
89
109
15
144
114
9
78|]
