{-# LANGUAGE StandaloneDeriving #-}

module Day6 (part1, part2) where

import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

data Race = Race
  { time :: Int,
    targetDistance :: Int
  }

deriving instance Show Race

holdToDistance :: Int -> Int -> Int
holdToDistance t h = (t - h) * h

-- Naive way...we can definitely do some elimination here
winningHoldTimes :: Race -> [Int]
winningHoldTimes (Race t d) = filter (> d) $ holdToDistance t <$> [1 .. t]

parseInput :: String -> [Race]
parseInput s = zipWith Race times distances
    where tS:dS:_ = lines s
          times = fmap read . filter (/="") . splitOn " " . fromJust . stripPrefix "Time: " $ tS
          distances = fmap read . filter (/="") . splitOn " " . fromJust . stripPrefix "Distance: " $ dS

combine :: Race -> Race -> Race
combine (Race t d) (Race tt dd) = Race (read (show t ++ show tt)) (read $ show d ++ show dd)

example = ["Time:      7  15   30", "Distance:  9  40  200"]

file = readFile "inputs/day6.txt"

part1 = do
  input <- file
  let races = parseInput input
  let possibleWins = fmap winningHoldTimes races
  let numberOfWins = product . fmap length $ possibleWins
  print numberOfWins 

part2 = do
  input <- file
  let races = parseInput input
  let realRace = foldl combine (Race 0 0) races 
  let possibleWins = winningHoldTimes realRace
  let numberOfWins = length possibleWins
  print numberOfWins 
