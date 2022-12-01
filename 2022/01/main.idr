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

data Error = Fe FileError | O

readGroups : (HasIO m) => String -> EitherT Error m (List1 (List Nat))
readGroups fileName = do
  c <- liftEither . mapFst Fe =<< lift (readFile fileName)
  let lines = split (== "") $ lines c
      nums : Maybe (List1 (List Nat))
      nums = traverse (traverse parsePositive) lines
  MkEitherT $ pure $ maybeToEither O $ traverse (traverse parsePositive) lines

part1 : IO ()
part1 = do
  Right groups <- runEitherT $ readGroups "real.txt"
    | Left e => printLn "oops"
  printLn . maximum $ sum <$> groups

part2 : IO ()
part2 = do
  Right groups <- runEitherT $ readGroups "real.txt"
    | Left e => printLn "oops"
  printLn . sum . List.take 3 . List.reverse . List.sort . toList $ sum <$> groups

