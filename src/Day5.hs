{-# LANGUAGE StandaloneDeriving #-}

module Day5 () where

import Data.List (find, findIndex, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Map = Map
  { source :: Int,
    destination :: Int,
    range :: Int
  }

deriving instance Show Map

transform :: Map -> Int -> Maybe Int
transform (Map s d r) n = match
  where
    distance = n - s
    match = if distance >= 0 && distance < r then Just (d + distance) else Nothing

transforms :: [Map] -> Int -> Int
transforms m n = eval tests
  where
    tests = flip transform n <$> m
    eval [] = n
    eval ((Just x) : xs) = x
    eval (Nothing : xs) = eval xs

pipeline :: [[Map]] -> [Int] -> [Int]
pipeline [] n = n
pipeline (x : xs) n = pipeline xs newResult
  where
    newResult = transforms x <$> n

lowest :: [Int] -> Int
lowest = minimum

parseMap :: String -> Map
parseMap s = Map so d r
  where
    [d, so, r] = read <$> splitOn " " s

parseMapSection :: String -> [Map]
parseMapSection = fmap parseMap . tail . lines

parseNumbers :: String -> [Int]
parseNumbers = fmap read . splitOn " " . fromJust . stripPrefix "seeds: "

parseInput :: String -> ([Int], [[Map]])
parseInput s = (numbers, maps)
  where
    line1 : _ : xs = lines s
    numbers = parseNumbers line1
    maps = parseMapSection <$> splitOn "\n\n" (unlines xs)

part1 = do
  rawS <- readFile "day5/input.txt"
  let (numbers, maps) = parseInput rawS

  print "part1"
  let res = pipeline maps numbers
  print $ lowest res

rangedNumbers :: [Int] -> [Int]
rangedNumbers [] = []
rangedNumbers (x : y : xs) = [x .. (x + (y - 1))] ++ rangedNumbers xs

part2 = do
  rawS <- readFile "day5/input.txt"
  let (numbers, maps) = parseInput rawS
  let ranged = rangedNumbers numbers

  print "part2"
  let res = pipeline maps ranged
  print $ lowest res

-- main = do
--   part1
--   part2
