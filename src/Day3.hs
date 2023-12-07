module Day3 (
    run
) where

import Control.Monad
import Data.Char (isDigit)
import Text.Read (readMaybe)

type Cord = (Int, Int)

data Digits = Digits
  { number :: Int,
    start :: Cord,
    end :: Cord
  }

instance Show Digits where
  show (Digits n s e) = "D" ++ show n ++ "-" ++ show s ++ "-" ++ show e

data Symbol = Symbol
  { symbol :: Char,
    pos :: Cord
  }

instance Show Symbol where
  show (Symbol s p) = "S" ++ show s ++ "-" ++ show p

-- S=1,3
-- Possible is 0,3|0,2|1,2|2,2|2,3|2,4|1,4|0,4
-- D=0,0,2
possibleMoves :: Cord -> [Cord]
possibleMoves x = moves <*> [x]
  where
    moves =
      [ \(a, b) -> (a - 1, b),
        \(a, b) -> (a - 1, b - 1),
        \(a, b) -> (a, b - 1),
        \(a, b) -> (a + 1, b - 1),
        \(a, b) -> (a + 1, b),
        \(a, b) -> (a + 1, b + 1),
        \(a, b) -> (a, b + 1),
        \(a, b) -> (a - 1, b + 1)
      ]

isDigitsNextToSymbol :: Digits -> Symbol -> Bool
isDigitsNextToSymbol (Digits _ start end) s = elem start validSpots || elem end validSpots
  where
    validSpots = possibleMoves (pos s)

isSymbolNextToDigit = flip isDigitsNextToSymbol

-- Filter digits to only those next to symbols
-- only need to compare for symbols above, below, and same row as current digit
validDigits :: [Digits] -> [Symbol] -> [Digits]
validDigits [] symbols = []
validDigits (d : xs) symbols = if or $ fmap (isDigitsNextToSymbol d) possibleSymbols then d : validDigits xs symbols else validDigits xs symbols
  where
    possibleSymbols = filter (\s -> fst (pos s) `elem` [fst (start d) - 1, fst (start d), fst (start d) + 1]) symbols

parseLine :: (Int, Int) -> String -> String -> ([Digits], [Symbol])
parseLine (r, c) prev "" = res <> ([], [])
  where
    beginCol = c - length prev
    res = case (readMaybe prev :: Maybe Int) of
      Just n -> ([Digits n (r, beginCol) (r, c - 1)], [])
      Nothing -> ([], [])
parseLine (r, c) prev (x : xs)
  | x == '.' = res <> parseLine (r, c + 1) "" xs
  | not . isDigit $ x = res <> ([], [Symbol x (r, c)]) <> parseLine (r, c + 1) "" xs
  | otherwise = parseLine (r, c + 1) (prev ++ [x]) xs
  where
    beginCol = c - length prev
    res = case (readMaybe prev :: Maybe Int) of
      Just n -> ([Digits n (r, beginCol) (r, c - 1)], [])
      Nothing -> ([], [])

getGearRatio :: [Symbol] -> [Digits] -> [Int]
getGearRatio [] _ = []
getGearRatio (x:xs) digits
    | length nextToDigits == 2 = ratio:getGearRatio xs digits
    | otherwise = getGearRatio xs digits
    where nextToDigits = filter (isSymbolNextToDigit x) digits
          ratio = product . fmap number $ nextToDigits

example =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

performPart1 :: String -> IO ()
performPart1 input = do
  let numberedLines = zip [0 ..]  . lines $ input 
  let (d, s) = mconcat $ fmap (\(n, l) -> parseLine (n, 0) "" l) numberedLines
  let res = validDigits d s
  let answer = sum $ number <$> res
  print answer

performPart2 :: String -> IO ()
performPart2 input = do 
  let numberedLines = zip [0 ..]  . lines $ input 
  let (d, s) = mconcat $ fmap (\(n,l) -> parseLine (n, 0) "" l) numberedLines
  let res = getGearRatio s d
  let answer2 = sum res
  print answer2

run = do
  let exampleString = unlines example
  performPart1 exampleString
  performPart2 exampleString

  fileString <- readFile "inputs/day3.txt"

  performPart1 fileString
  performPart2 fileString
