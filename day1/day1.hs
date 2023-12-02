import Data.Char (digitToInt, isDigit)
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))

mapDigitString "one" = 1
mapDigitString "two" = 2
mapDigitString "three" = 3

isDigitString t = False

type RawCode = String
type Code = Int

readPuzzleInput :: RawCode -> Maybe [Code]
readPuzzleInput x = do
  let xLines = lines x
  sequence $ filter isJust $ parseCode <$> xLines

readInt :: Char -> Maybe Int
readInt x = if isDigit x then Just $ digitToInt x else Nothing

readDigitString :: String -> Maybe Int
readDigitString x = if isDigitString x then Just $ mapDigitString x else Nothing

readFirstInt :: String -> Maybe Int
readFirstInt [] = Nothing
readFirstInt (x:xs) = case readInt x of
    Just int -> Just int
    Nothing -> readFirstInt xs

readFirstIntFromText :: String -> Maybe Int
readFirstIntFromText s = Nothing


-- Find first and last digit in string
parseCode :: RawCode -> Maybe Code
parseCode code = do
    first <- readFirstInt code <|> readFirstIntFromText code
    second <- (readFirstInt . reverse) code <|> (readFirstIntFromText . reverse) code
    Just $ first * 10 + second 
    

combineCodes :: [Code] -> Int
combineCodes = sum

example = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]
example2 = [ "two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen" ]

main = do
    let res = parseCode <$> example2
    print res
  -- rawFile <- readFile "day1/input.txt"
  -- let codes = readPuzzleInput rawFile
  -- case codes of
  --   Just c -> print $ combineCodes c
  --   Nothing -> print "Failed to read codes"
