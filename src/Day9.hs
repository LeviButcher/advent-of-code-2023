module Day9 (example, input, diff, nextInSeries, part1, prevInSeries, part2) where

nextInSeries :: [Int] -> Int
nextInSeries = sum . fmap last . takeTillBase . iterate diff
  where
    takeTillBase = takeWhile (not . all (== 0))

prevInSeries :: [Int] -> Int
prevInSeries = foldr1 (-) . fmap head . takeTillBase . iterate diff
  where
    takeTillBase = takeWhile (not . all (== 0))

diff :: [Int] -> [Int]
diff [] = []
diff [_] = []
diff (x : y : xs) = y - x : diff (y : xs)

parse :: String -> [[Int]]
parse s = n
  where
    l = lines s
    n = fmap read . words <$> l

part1 :: String -> IO Int
part1 s = do
  let i = parse s
  let res = sum $ nextInSeries <$> i
  pure res

part2 :: String -> IO Int
part2 s = do
  let i = parse s
  let res = sum $ prevInSeries <$> i
  pure res

example :: IO String
example = readFile "./inputs/day9.example.txt"

input :: IO String
input = readFile "./inputs/day9.txt"
