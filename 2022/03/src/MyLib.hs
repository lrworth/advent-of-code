{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module MyLib where

import Control.Monad (guard)
import Data.Foldable (find)
import Data.List (group, nub, sort)
import Numeric.Natural
import System.IO (readFile')

data Case = Upper | Lower
  deriving (Eq, Ord, Show)

data Item = Item Case Natural
  deriving (Eq, Ord, Show)

mkItem :: Char -> Maybe Item
mkItem ch =
  if
      | ch `elem` ['a' .. 'z'] -> Just $ Item Lower (fromIntegral (fromEnum ch) - fromIntegral (fromEnum 'a'))
      | ch `elem` ['A' .. 'Z'] -> Just $ Item Upper (fromIntegral (fromEnum ch) - fromIntegral (fromEnum 'A'))
      | otherwise -> Nothing

itemPriority :: Item -> Natural
itemPriority (Item c ch) =
  case c of
    Lower -> ch + 1
    Upper -> ch + 27

newtype Rucksack = Rucksack ([Item], [Item])
  deriving (Show)

parseRucksack :: String -> Maybe Rucksack
parseRucksack s = do
  allItems <- traverse mkItem s
  let numItems = length allItems
  guard $ even numItems
  pure . Rucksack $ splitAt (numItems `div` 2) allItems

rucksackCommonItem :: Rucksack -> Maybe Item
rucksackCommonItem (Rucksack (as, bs)) =
  find (`elem` bs) as

part1 :: IO (Maybe Natural)
part1 = do
  Just rucksacks <- traverse parseRucksack . lines <$> readFile' "real.txt"
  pure $ sum <$> traverse (fmap itemPriority . rucksackCommonItem) rucksacks

newtype ElfGroup = ElfGroup ([Item], [Item], [Item])
  deriving (Show)

elfGroupTripleItem :: ElfGroup -> Maybe Item
elfGroupTripleItem (ElfGroup (as, bs, cs)) =
  fmap head
    . find ((== 3) . length)
    . group
    . sort
    $ nub as ++ nub bs ++ nub cs

parseElfGroups :: String -> Maybe [ElfGroup]
parseElfGroups = go . lines
  where
    go :: [String] -> Maybe [ElfGroup]
    go (a : b : c : rest) = do
      a' <- traverse mkItem a
      b' <- traverse mkItem b
      c' <- traverse mkItem c
      rest' <- go rest
      pure $ ElfGroup (a', b', c') : rest'
    go _ = pure []

part2 :: IO (Maybe Natural)
part2 = do
  Just elfGroups <- parseElfGroups <$> readFile' "real.txt"
  pure $ sum <$> traverse (fmap itemPriority . elfGroupTripleItem) elfGroups
