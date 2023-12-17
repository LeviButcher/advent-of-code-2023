module Day7
  ( example,
    input,
    card,
    game,
    games,
  )
where

import Data.List (group, sort, sortBy)
import Text.ParserCombinators.Parsec

newtype Card = Card Char
  deriving (Show)

instance Eq Card where
  Card a == Card b = a == b

cardStrength :: Card -> Int
cardStrength (Card 'A') = 14
cardStrength (Card 'K') = 13
cardStrength (Card 'Q') = 12
cardStrength (Card 'J') = 11
cardStrength (Card 'T') = 10
cardStrength (Card '9') = 9
cardStrength (Card '8') = 8
cardStrength (Card '7') = 7
cardStrength (Card '6') = 6
cardStrength (Card '5') = 5
cardStrength (Card '4') = 4
cardStrength (Card '3') = 3
cardStrength (Card '2') = 2
cardStrength (Card '1') = 1
cardStrength _ = error "invalid char"

newtype Hand = Hand [Card]
  deriving (Show)

instance Eq Hand where
  Hand a == Hand b = a == b

-- instance Ord Hand where
--   -- Hand a `compare` Hand b = case (aList, bList) of
--   --   ((s1, c1):(s2, c2):_, ():():_) -> GT
--     where aList = uniq . fmap cardStrength $ a
--           bList = uniq . fmap cardStrength $ b
--
type Bet = Int

type Game = (Bet, Hand)

card :: GenParser Char st Card
card = faceC <|> digitC
  where
    faceC = Card <$> satisfy (`elem` "AKQJT")
    digitC = Card <$> satisfy (`elem` "23456789")

game :: GenParser Char st Game
game = do
  cards <- manyTill card space
  let cards' = sortBy (flip (\a b -> cardStrength a `compare` cardStrength b)) cards
  bet <- read <$> many digit
  _ <- newline
  pure (bet, Hand cards')

games :: GenParser Char st [Game]
games = manyTill game eof

uniq :: (Eq a) => [a] -> [(a, Int)]
uniq = fmap (\f -> (head f, length f)) . group

-- Hand->Result -> Sort by win strength -> multiple by (index+1) * bet -> sum

example :: IO String
example = readFile "inputs/day7.example.txt"

input :: IO String
input = readFile "inputs/day7.txt"

-- part1 :: IO ()
-- part1 = do
--   i <- example
--   case parse games "" i of
--     Left err -> print err
--     Right gg -> print $ sortBy (\(_,a) (_, b) -> a `compare` b) gg
