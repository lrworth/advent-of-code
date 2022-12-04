{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module MyLib where

import Data.Void
import Numeric.Natural
import System.IO (readFile')
import Text.Megaparsec (MonadParsec, parseMaybe)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

data Assignment = Assignment Natural Natural
  deriving (Show)

assignmentP :: MonadParsec e String m => m Assignment
assignmentP = do
  start <- decimal
  _ <- char '-'
  end <- decimal
  pure $ Assignment start end

parser :: MonadParsec e String m => m (Assignment, Assignment)
parser = do
  a1 <- assignmentP
  _ <- char ','
  a2 <- assignmentP
  pure (a1, a2)

parseFile :: String -> IO [(Assignment, Assignment)]
parseFile fileName = do
  assignmentPairsS <- lines <$> readFile' fileName
  Just assignmentPairs <- pure $ traverse (parseMaybe @Void parser) assignmentPairsS
  pure assignmentPairs

assignmentContains :: Assignment -> Assignment -> Bool
assignmentContains (Assignment a1start a1end) (Assignment a2start a2end) = a1start <= a2start && a2end <= a1end

pairHasRedundantAssignment :: (Assignment, Assignment) -> Bool
pairHasRedundantAssignment (a1, a2) =
  a1 `assignmentContains` a2 || a2 `assignmentContains` a1

part1 :: IO Int
part1 =
  length . filter pairHasRedundantAssignment <$> parseFile "real.txt"

assignmentOverlaps :: Assignment -> Assignment -> Bool
assignmentOverlaps (Assignment a1start a1end) (Assignment a2start a2end) =
  a2start <= a1end && a1start <= a2end

part2 :: IO Int
part2 =
  length . filter (uncurry assignmentOverlaps) <$> parseFile "real.txt"
