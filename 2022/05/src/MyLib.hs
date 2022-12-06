{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib where

import Control.Applicative ((<|>))
import Control.Monad (guard, replicateM_)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Void
import System.IO (readFile')
import Text.Megaparsec (MonadParsec, eof, many, parseMaybe, try)
import Text.Megaparsec.Char (char, string, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)

newtype Crate = Crate Char
  deriving (Show)

newtype Stack = Stack [Crate]
  deriving (Show)

data Instruction = Instruction
  { quantity :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

data Input = Input
  { stacks :: Map Int Stack,
    procedure :: [Instruction]
  }
  deriving (Show)

crateP :: MonadParsec e String m => m (Maybe Crate)
crateP =
  ( Just . Crate <$> (char '[' *> upperChar <* char ']')
      <|> Nothing <$ (char ' ' <* char ' ' <* char ' ')
  )
    <* (try (() <$ char ' ') <|> eof)

stackSliceP :: MonadParsec e String m => m [Maybe Crate]
stackSliceP = do
  many crateP

stacksFromSlices :: [[Maybe Crate]] -> Map Int Stack
stacksFromSlices = M.fromList . zip [1 ..] . fmap (Stack . catMaybes) . transpose

parseStacks :: [String] -> Maybe (Map Int Stack)
parseStacks = fmap stacksFromSlices . traverse (parseMaybe @Void stackSliceP)

instructionP :: MonadParsec e String m => m Instruction
instructionP = do
  _ <- string "move "
  quantity <- decimal
  _ <- string " from "
  from <- decimal
  _ <- string " to "
  to <- decimal
  pure Instruction {..}

parseProcedure :: [String] -> Maybe [Instruction]
parseProcedure = traverse (parseMaybe @Void instructionP)

parseInput :: String -> Maybe Input
parseInput i = do
  (stacksSection, _ : procedureSection) <- pure . break null $ lines i
  guard . all (\c -> isSpace c || isDigit c) $ last stacksSection
  stacks <- parseStacks $ init stacksSection
  procedure <- parseProcedure procedureSection
  pure Input {..}

step :: Instruction -> StateT (Map Int Stack) Maybe ()
step Instruction {..} = replicateM_ quantity $ do
  stacks <- get
  Stack (pick : fromStack) <- lift $ M.lookup from stacks
  Stack toStack <- lift $ M.lookup to stacks
  put
    . M.insert to (Stack (pick : toStack))
    . M.insert from (Stack fromStack)
    $ stacks

part1 :: IO String
part1 = do
  s <- readFile' "real.txt"
  Just Input {..} <- pure $ parseInput s
  Just finalStacks <- pure $ execStateT (traverse step procedure) stacks
  traverse
    ( \stack -> do
        Stack (Crate c : _) <- pure stack
        pure c
    )
    $ M.elems finalStacks

step2 :: Instruction -> StateT (Map Int Stack) Maybe ()
step2 Instruction {..} = do
  stacks <- get
  Stack fromStack <- lift $ M.lookup from stacks
  Stack toStack <- lift $ M.lookup to stacks
  let (pick, fromRest) = splitAt quantity fromStack
  put
    . M.insert to (Stack (pick ++ toStack))
    . M.insert from (Stack fromRest)
    $ stacks

part2 :: IO String
part2 = do
  s <- readFile' "real.txt"
  Just Input {..} <- pure $ parseInput s
  Just finalStacks <- pure $ execStateT (traverse step2 procedure) stacks
  traverse
    ( \stack -> do
        Stack (Crate c : _) <- pure stack
        pure c
    )
    $ M.elems finalStacks
