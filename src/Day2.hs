module Day2 (
    run
) where

import Data.List (stripPrefix)
import Data.List.Split
import Data.Maybe (fromJust)

data Cubes = Cubes
  { red :: Int,
    green :: Int,
    blue :: Int
  }

instance Show Cubes where
  show (Cubes r g b) = "R" ++ show r ++ "-G" ++ show g ++ "-B" ++ show b

instance Semigroup Cubes where
  (Cubes r g b) <> (Cubes rr gg bb) = Cubes (r+rr) (g + gg) (b + bb)

instance Monoid Cubes where
  mempty = Cubes 0 0 0

validGameSubset :: Cubes -> [Cubes] -> Bool
validGameSubset m = all (validDraw m)

minimumBag :: [Cubes] -> Cubes
minimumBag x = Cubes maxR maxG maxB
  where
    maxR = maximum . fmap red $ x
    maxG = maximum . fmap green $ x
    maxB = maximum . fmap blue $ x

powerCube :: Cubes -> Int
powerCube x = red x * green x * blue x

validDraw :: Cubes -> Cubes -> Bool
validDraw m draw = (red m >= red draw) && (green m >= green draw) && (blue m >= blue draw)

parseGameLine :: String -> (Int, [Cubes])
parseGameLine s = (gameNumber, parseDrawSeries series)
  where
    [game, series] = splitOn ": " s
    gameNumber = read . fromJust . stripPrefix "Game " $ game

parseDrawSeries :: String -> [Cubes]
parseDrawSeries = fmap parseDraws . splitOn "; "

parseDraws :: String -> Cubes
parseDraws = mconcat . fmap parseCube . splitOn ", "

parseCube :: String -> Cubes
parseCube s = drawToCube (read n) color
  where
    [n, color] = splitOn " " s

drawToCube :: Int -> String -> Cubes
drawToCube n "red" = Cubes n 0 0
drawToCube n "green" = Cubes 0 n 0
drawToCube n "blue" = Cubes 0 0 n

examples =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

run = do
  let bag = Cubes 12 13 14
  let gameSeries = parseGameLine <$> examples

  rawInput <- readFile "inputs/day2.txt"

  let gameSeries = parseGameLine <$> lines rawInput
  let res = fmap (validGameSubset bag) <$> gameSeries
  let answer = sum $ fst <$> filter snd res
  let minimumCubes = minimumBag . snd <$> gameSeries
  let answer2 = sum . fmap powerCube $ minimumCubes

  print answer
  print answer2
