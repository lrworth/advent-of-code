{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module MyLib where

import Control.Exception (assert)
import Control.Monad (guard)
import Data.Foldable (find)
import Data.List (group, nub, sort)
import Data.Proxy (Proxy (..))
import Data.Vector.Fixed (Arity, VecList, convert, fromListM)
import GHC.TypeLits (Nat, natVal)
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

splitInto :: Arity n => Proxy (n :: Nat) -> [a] -> [VecList n a]
splitInto proxy l =
  let splitSize = fromEnum $ natVal proxy
      (front, back) = splitAt splitSize l
   in case fromListM front of
        Nothing -> assert (null back) []
        Just frontVec -> frontVec : splitInto proxy back

parseElfGroups :: String -> Maybe [ElfGroup]
parseElfGroups = traverse parseElfGroup . splitInto (Proxy :: Proxy 3) . lines
  where
    parseElfGroup :: VecList 3 String -> Maybe ElfGroup
    parseElfGroup vl = case convert vl of
      (a, b, c) -> do
        a' <- traverse mkItem a
        b' <- traverse mkItem b
        c' <- traverse mkItem c
        pure $ ElfGroup (a', b', c')

part2 :: IO (Maybe Natural)
part2 = do
  Just elfGroups <- parseElfGroups <$> readFile' "real.txt"
  pure $ sum <$> traverse (fmap itemPriority . elfGroupTripleItem) elfGroups
