{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.Error.Hoist
import Control.Monad.Except
import Control.Monad.State
import Data.Data
import Data.Generics.Labels ()
import Data.List (isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import Data.Monoid
import GHC.Generics hiding (to)
import System.IO (readFile')
import Text.Read (readMaybe)
import Text.Show.Pretty
import Witherable

data NodeType = Dir | File Int
  deriving (Show)

data Command = Cd String | Ls [(String, NodeType)]
  deriving (Show)

parseLsOutputLine :: String -> Maybe (String, NodeType)
parseLsOutputLine =
  ( \case
      ["dir", name] -> pure (name, Dir)
      [sizeStr, name] -> do
        size <- readMaybe sizeStr
        pure (name, File size)
      _ -> Nothing
  )
    . words

parseCommand :: [String] -> [String] -> Maybe Command
parseCommand commandWords outputLines =
  case commandWords of
    ["cd", dir] -> assert (null outputLines) $ pure $ Cd dir
    ["ls"] -> Ls <$> traverse parseLsOutputLine outputLines
    _ -> Nothing

parseCommands :: String -> Maybe [Command]
parseCommands = go . lines
  where
    go :: [String] -> Maybe [Command]
    go [] = pure []
    go (commandLine : rest) = do
      "$" : commandWords <- pure $ words commandLine
      (outputLines, rest') <- pure $ break ("$" `isPrefixOf`) $ rest
      command <- parseCommand commandWords outputLines
      commands <- go rest'
      pure $ command : commands

data FsNode = FNDir (M.Map String FsNode) | FNFile Int
  deriving (Data, Generic, Show)

instance Plated FsNode

foldMapFsNode :: Monoid m => (String -> FsNode -> m) -> (Int -> m) -> FsNode -> m
foldMapFsNode onDir onFile = \case
  FNDir m -> M.foldMapWithKey onDir m
  FNFile i -> onFile i

unionFsNodes :: FsNode -> FsNode -> Maybe FsNode
unionFsNodes (FNDir map1) (FNDir map2) =
  FNDir
    <$> M.mergeA
      M.preserveMissing
      M.preserveMissing
      (M.zipWithAMatched $ \_ -> unionFsNodes)
      map1
      map2
unionFsNodes _ _ = Nothing

bury :: [String] -> FsNode -> FsNode
bury = appEndo . foldMap (\p -> Endo $ FNDir . M.singleton p) . reverse

fsDirSize :: M.Map String FsNode -> Int
fsDirSize fd = getSum $ foldMap (Sum . \case FNDir m -> fsDirSize m; FNFile s -> s) fd

data Learning = Learning
  { pwd :: [String],
    fileSystem :: FsNode
  }
  deriving (Generic, Show)

fsNodeFromDirEntries :: [(String, NodeType)] -> FsNode
fsNodeFromDirEntries =
  FNDir
    . fmap
      ( \case
          Dir -> FNDir M.empty
          File n -> FNFile n
      )
    . M.fromList

data CommandError = UpFromTop | Conflict
  deriving (Show)

execCommand :: (MonadState Learning m, MonadError CommandError m) => Command -> m ()
execCommand = \case
  Cd "/" -> #pwd .= []
  Cd ".." -> do
    _ :| ps <- uses #pwd NEL.nonEmpty <!?> UpFromTop
    #pwd .= ps
  Cd dir -> modifying #pwd (dir :)
  Ls entries -> do
    fs <- use #fileSystem
    p <- use #pwd
    newFs <- unionFsNodes fs (bury p $ fsNodeFromDirEntries entries) <?> Conflict
    assign #fileSystem newFs

readFileSystem :: FilePath -> IO FsNode
readFileSystem path = do
  inputStr <- readFile' path
  Just commands <- pure $ parseCommands inputStr
  Right Learning {fileSystem} <-
    runExceptT $
      execStateT
        (traverse execCommand commands)
        (Learning {pwd = [], fileSystem = FNDir M.empty})
  pure fileSystem

part1 :: IO Int
part1 = do
  fileSystem <- readFileSystem "real.txt"
  pure $
    fileSystem
      & sumOf
        ( cosmos
            . #_FNDir
            . to fsDirSize
            . filtered (<= 100000)
        )

part2 :: IO Int
part2 = do
  input <- readFile' "real.txt"
  undefined
