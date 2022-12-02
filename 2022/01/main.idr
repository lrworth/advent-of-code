module Main

import System.File.ReadWrite
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Ord
import Data.String
import Data.Either
import Data.List1

maximum : Ord m => List1 m -> m
maximum = foldl1 ((<+>) @{Maximum})

data Error = EFileError FileError | EParseError

Show Error where
  show (EFileError fe) = "EFileError " ++ show fe
  show EParseError = "EParseError"

readGroups : (HasIO m, MonadError Error m) => String -> m (List1 (List Nat))
readGroups fileName = do
  eContents <- liftIO (readFile fileName)
  contents <- liftEither . mapFst EFileError $ eContents
  liftEither . maybeToEither EParseError . traverse (traverse parsePositive) . split (== "") . lines $ contents

part1 : IO ()
part1 = do
  Right groups <- runEitherT $ readGroups {m = _ Error IO} "real.txt"
    | Left e => printLn e
  printLn . maximum $ sum <$> groups

part2 : IO ()
part2 = do
  Right groups <- runEitherT $ readGroups {m = _ Error IO} "real.txt"
    | Left e => printLn e
  printLn . sum . List.take 3 . List.reverse . List.sort . toList $ sum <$> groups

