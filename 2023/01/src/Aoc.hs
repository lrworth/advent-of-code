{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Aoc where

import Data.Char qualified as Char
import Data.Map qualified as Map
import Data.Text qualified as Text
import Path (File, Path, Rel, relfile, toFilePath)
import Relude.Unsafe (fromJust)
import Text.Megaparsec (Parsec, anySingle, chunk, parse, parseTest, single, try)

input :: Path Rel File -> IO [Text]
input = fmap (lines . decodeUtf8) . readFileBS . toFilePath

readTextMaybe :: (Read a) => Text -> Maybe a
readTextMaybe = readMaybe . toString

firstDigit :: Text -> Maybe Char
firstDigit = Text.find Char.isDigit

lastDigit :: Text -> Maybe Char
lastDigit = firstDigit . Text.reverse

asTwoDigitNumber :: Text -> Maybe Int
asTwoDigitNumber t = do
  a <- Char.digitToInt <$> firstDigit t
  b <- Char.digitToInt <$> lastDigit t
  pure $ 10 * a + b

solveOne :: [Text] -> Maybe Int
solveOne = fmap sum . traverse asTwoDigitNumber

goSampleOne :: IO Int
goSampleOne = fromJust . solveOne <$> input [relfile|input/sampleOne.txt|]

goInputOne :: IO Int
goInputOne = fromJust . solveOne <$> input [relfile|input/inputOne.txt|]

digitNames :: Map Char Text
digitNames =
  [ ('0', "zero"),
    ('1', "one"),
    ('2', "two"),
    ('3', "three"),
    ('4', "four"),
    ('5', "five"),
    ('6', "six"),
    ('7', "seven"),
    ('8', "eight"),
    ('9', "nine")
  ]

revDigitNames :: Map Char Text
revDigitNames = Text.reverse <$> digitNames

pDigit :: Map Char Text -> Parsec Void Text Char
pDigit = getAlt . Map.foldMapWithKey (\d n -> Alt $ try (chunk n *> pure d) <|> single d)

testPDigit :: IO ()
testPDigit = do
  parseTest (pDigit digitNames) "two"
  parseTest (pDigit digitNames) "twof"
  parseTest (pDigit digitNames) "ftwo"
  parseTest (pDigit digitNames) "-1"

pFirstDigit :: Map Char Text -> Parsec Void Text Char
pFirstDigit dn = try (pDigit dn) <|> (anySingle *> pFirstDigit dn)

testPFirstDigit :: IO ()
testPFirstDigit = do
  parseTest (pFirstDigit digitNames) "two"
  parseTest (pFirstDigit digitNames) "twof"
  parseTest (pFirstDigit digitNames) "ftwone"
  parseTest (pFirstDigit digitNames) "3ftwone"
  parseTest (pFirstDigit digitNames) "-1"
  parseTest (pFirstDigit revDigitNames) (Text.reverse "ftwone")

asTwoDigitNumber' :: Text -> Maybe Int
asTwoDigitNumber' t = do
  a <- Char.digitToInt <$> rightToMaybe (parse (pFirstDigit digitNames) "" t)
  b <- Char.digitToInt <$> rightToMaybe (parse (pFirstDigit revDigitNames) "" (Text.reverse t))
  pure $ a * 10 + b

solveTwo :: [Text] -> Maybe Int
solveTwo = fmap sum . traverse asTwoDigitNumber'

goSampleTwo :: IO Int
goSampleTwo = fromJust . solveTwo <$> input [relfile|input/sampleTwo.txt|]

goInputTwo :: IO Int
goInputTwo = fromJust . solveTwo <$> input [relfile|input/inputOne.txt|]
