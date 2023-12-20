module Day7
  ( example,
    input,
    card,
    game,
    games,
    cardValue,
    part1,
    Hand (..),
    uniq,
    test,
    validExample,
  )
where

import Data.Char (digitToInt)
import Data.List (find, nub, sortBy)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

type Card = Char

data Hand = Hand {cards :: ![Card], handType :: !HandType}
  deriving (Eq, Show)

instance Ord Hand where
  compare h hh
    | t == tt = firstBetterCard (cards h) (cards hh)
    | otherwise = t `compare` tt
    where
      t = handType h
      tt = handType hh
      firstBetterCard :: [Card] -> [Card] -> Ordering
      firstBetterCard [] [] = EQ
      firstBetterCard _ [] = error "Impossible case"
      firstBetterCard [] _ = error "Impossible case"
      firstBetterCard (x : xs) (y : ys) =
        if cardValue x == cardValue y
          then firstBetterCard xs ys
          else cardValue x `compare` cardValue y

type Bet = Int

type Game = (Bet, Hand)

card :: GenParser Char st Card
card = faceC <|> digitC
  where
    faceC = satisfy (`elem` "AKQJT")
    digitC = satisfy (`elem` "23456789")

game :: GenParser Char st Game
game = do
  cards' <- manyTill card space
  bet <- manyTill digit newline
  pure (read bet, Hand cards' (getHandType cards'))

games :: GenParser Char st [Game]
games = manyTill game eof

--   Part1 values
-- cardValue :: Card -> Int
-- cardValue 'A' = 14
-- cardValue 'K' = 13
-- cardValue 'Q' = 12
-- cardValue 'J' = 11
-- cardValue 'T' = 10
-- cardValue c
--   | c `elem` ['2' .. '9'] = digitToInt c
--   | otherwise = error "HUGE ERROR"
--   Part2 values
cardValue :: Card -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = 1
cardValue 'T' = 10
cardValue c
  | c `elem` ['2' .. '9'] = digitToInt c
  | otherwise = error "HUGE ERROR"

data HandType = Highcard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Eq, Ord, Show)

flipCompare = flip compare

getHandType :: [Card] -> HandType
getHandType h
  | jCount == 5 = FiveKind -- Lazily avoid exception case of indexing array
  | fj == 5 = FiveKind
  | fj == 4 = FourKind
  | fj == 3 && sj == 2 = FullHouse
  | fj == 3 = ThreeKind
  | fj == 2 && sj == 2 = TwoPair
  | fj == 2 = OnePair
  | jCount > 0 = OnePair
  | otherwise = Highcard
  where
    allCounts = uniq h
    counts = sortBy (\(_, a) (_, b) -> a `flipCompare` b) $ filter ((/= 'J') . fst) allCounts
    (_, jCount) = fromMaybe ('J', 0) $ find ((== 'J') . fst) allCounts
    (_, fc) = head counts
    (_, sc) = counts !! 1
    fj = fc + jCount
    sj = sc

uniq :: (Eq t) => [t] -> [(t, Int)]
uniq x = (\y -> (y, length . filter (== y) $ x)) <$> g
  where
    g = nub x

sortHands :: [Game] -> [Game]
sortHands = sortBy (\a b -> snd a `compare` snd b)

totalWinnings :: [Game] -> Int
totalWinnings h = sum earnings
  where
    bets = fmap fst . sortHands $ h
    earnings = zipWith (*) bets [1 ..]

part1 :: String -> IO Int
part1 s = do
  let res = runParser games () "" s
  pure $ case res of
    Right hands -> totalWinnings hands
    Left _ -> -1

test :: IO ()
test = do
  i <- example
  let res = runParser games () "" i
  case res of
    Right hands -> writeFile ".temp" (unlines . fmap show . sortHands $ hands)
    Left _ -> pure ()

example :: IO String
example = readFile "inputs/day7.example.txt"

validExample :: IO ()
validExample = do
  res <- example >>= part1
  print $ 6839 `compare` res

input :: IO String
input = readFile "inputs/day7.txt"
