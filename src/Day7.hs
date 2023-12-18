module Day7
  ( example,
    input,
    card,
    game,
    games,
    cardValue,
    part1,
    compareHand,
    handType,
    firstBetterCard,
    HandType (..),
    uniq,
    test,
  )
where

import Data.List (nub, sortBy)
import Text.ParserCombinators.Parsec

type Card = Char

type Hand = [Card]

type Bet = Int

type Game = (Bet, Hand)

cardValue :: Card -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = 11
cardValue 'T' = 10
cardValue '9' = 9
cardValue '8' = 8
cardValue '7' = 7
cardValue '6' = 6
cardValue '5' = 5
cardValue '4' = 4
cardValue '3' = 3
cardValue '2' = 2
cardValue _ = -1

data HandType = Highcard | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

handType :: Hand -> HandType
handType h
  | fc == 5 = FiveKind
  | fc == 4 = FourKind
  | fc == 3 && sc == 2 = FullHouse
  | fc == 3 = ThreeKind
  | fc == 2 = TwoPair
  | otherwise = Highcard
  where
    counts = sortBy (flip compare) . fmap snd . uniq $ h
    fc = counts !! 0
    sc = counts !! 1

compareHand :: Hand -> Hand -> Ordering
compareHand h hh
  | t == tt = firstBetterCard h hh
  | t > tt = GT
  | otherwise = LT
  where
    t = handType h
    tt = handType hh

firstBetterCard :: Hand -> Hand -> Ordering
firstBetterCard [] [] = EQ
firstBetterCard (x : xs) (y : ys)
  | cardValue x > cardValue y = GT
  | cardValue x < cardValue y = LT
  | otherwise = firstBetterCard xs ys

uniq :: (Eq t) => [t] -> [(t, Int)]
uniq x = (\y -> (y, length . filter (== y) $ x)) <$> g
  where
    g = nub x

card :: GenParser Char st Card
card = faceC <|> digitC
  where
    faceC = satisfy (`elem` "AKQJT")
    digitC = satisfy (`elem` "23456789")

game :: GenParser Char st Game
game = do
  cards <- manyTill card space
  bet <- manyTill digit newline
  pure (read bet, cards)

games :: GenParser Char st [Game]
games = manyTill game eof

sortHands :: [Game] -> [Game]
sortHands = sortBy (\a b -> snd a `compareHand` snd b)

totalWinnings :: [Game] -> Int
totalWinnings h = sum earnings
  where
    ranked = fmap fst . sortHands $ h
    earnings = zipWith (*) ranked [1 ..]

part1 :: String -> IO Int
part1 s = do
  let res = runParser games () "" s
  pure $ case res of
    Right hands -> totalWinnings hands
    Left _ -> -1

test :: IO ()
test = do
  i <- input
  let res = runParser games () "" i
  case res of
    Right hands -> writeFile ".temp" (unlines . fmap (show . (\(a, b) -> (handType b, a, b))) . sortHands $ hands)
    Left _ -> pure ()

example :: IO String
example = readFile "inputs/day7.example.txt"

input :: IO String
input = readFile "inputs/day7.txt"
