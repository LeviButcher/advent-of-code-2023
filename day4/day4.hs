{-# LANGUAGE StandaloneDeriving #-}

import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Card = Card
  { cID :: Int,
    expected :: [Int],
    numbers :: [Int],
    original :: Bool
  }

instance Eq Card where
  c == c2 = cID c == cID c2

instance Ord Card where
  c `compare` c2 = cID c `compare` cID c2

deriving instance Show Card

score :: Card -> Int
score c@(Card id winning nums _)
  | matchCount > 0 = 2 ^ (matchCount - 1)
  | otherwise = 0
  where
    matchCount = length $ matches c

matches :: Card -> [Int]
matches (Card _ winning nums _) = winning `intersect` nums

copy :: Card -> Card
copy (Card i e n o) = Card i e n False

parseNumbers :: String -> [Int]
parseNumbers = fmap read . filter (not . null) . splitOn " "

parseCard :: String -> Card
parseCard s = Card (read sId) (parseNumbers sExpected) (parseNumbers sNumbers) True
  where
    removePrefix = fromJust $ stripPrefix "Card" s
    [sId, rest] = splitOn ": " removePrefix
    [sExpected, sNumbers] = splitOn " | " rest

findCard :: Int -> [Card] -> Maybe Card
findCard n = find ((== n) . cID)

cardsWon :: [Card] -> [Card]
cardsWon [] = []
cardsWon (c : xs) = c : cardsWon (won ++ xs)
  where
   won = copiesWon c xs

copiesWon :: Card -> [Card] -> [Card]
copiesWon c x = copies
  where
    copyAmount = length . matches $ c
    copies = fmap copy . take copyAmount . filter (\cc -> cc > c && original cc) $ x


answer1 :: String -> IO ()
answer1 s = do
  let cards = parseCard <$> lines s
  let answer = foldr ((+) . score) 0 cards
  print answer

answer2 :: String -> IO ()
answer2 s = do
  let cards = parseCard <$> lines s
  let won = cardsWon cards
  print (length won)

example1 =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

example2 =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

main = do
  answer1 (unlines example1)
  answer2 (unlines example2)

  input <- readFile "input.txt"
  answer1 input
  answer2 input
