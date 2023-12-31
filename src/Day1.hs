module Day1 (
    readPuzzleInput,
    parseDigits,
    parsePrefix,
    parseCode,
    combineCodes,
    run
) where

import Data.Maybe (isJust)
import Data.List (tails)

type RawCode = String
type Code = Int

readPuzzleInput :: RawCode -> Maybe [Code]
readPuzzleInput x = sequence . filter isJust $ parseCode <$> lines x

parseDigits :: String -> [Int]
parseDigits x = tails x >>= parsePrefix

-- Cursed but works :)
parsePrefix :: String -> [Int]
parsePrefix ('o':'n':'e':xs) = [1]
parsePrefix ('1':xs) = [1]
parsePrefix ('t':'w':'o':xs) = [2]
parsePrefix ('2':xs) = [2]
parsePrefix ('t':'h':'r':'e':'e':xs) = [3]
parsePrefix ('3':xs) = [3]
parsePrefix ('f':'o':'u':'r':xs) = [4]
parsePrefix ('4':xs) = [4]
parsePrefix ('f':'i':'v':'e':xs) = [5]
parsePrefix ('5':xs) = [5]
parsePrefix ('s':'i':'x':xs) = [6]
parsePrefix ('6':xs) = [6]
parsePrefix ('s':'e':'v':'e':'n':xs) = [7]
parsePrefix ('7':xs) = [7]
parsePrefix ('e':'i':'g':'h':'t':xs) = [8]
parsePrefix ('8':xs) = [8]
parsePrefix ('n':'i':'n':'e':xs) = [9]
parsePrefix ('9':xs) = [9]
parsePrefix _ = []


-- Find first and last digit in string
parseCode :: RawCode -> Maybe Code
parseCode code = do
    let digits = parseDigits code
    case digits of
        [] -> Nothing
        xs -> Just $ head xs * 10 + last xs


combineCodes :: [Code] -> Int
combineCodes = sum


run :: IO ()
run = do
  rawFile <- readFile "inputs/day1.txt"
  let codes = readPuzzleInput rawFile
  case codes of
    Just c -> print $ combineCodes c
    Nothing -> print "Failed to read codes"

