module Day8 (example, input, buildTree, traverseTree, traverseTill, part1, run, part2, lcmm) where

import Control.Applicative ((<|>))
import Data.List (find, isSuffixOf)
import Text.Parsec (anyChar, eof, manyTill, newline, runParser, space)
import Text.Parsec.Char (char)
import Text.Parsec.String (GenParser)

type Key = String

data Node = Node
  { key :: Key,
    left :: (Maybe Node),
    right :: (Maybe Node)
  }
  deriving (Show, Eq)

data Path = R | L
  deriving (Show, Eq)

type RawNode = (Key, (Key, Key))

-- Infinite Structure
buildTree :: Key -> [RawNode] -> Maybe Node
buildTree start x = do
  (k, (l, r)) <- find ((== start) . fst) x
  Just $ Node k (buildTree l x) (buildTree r x)

traverseTree :: Path -> Node -> Maybe Node
traverseTree L = left
traverseTree R = right

traverseTill :: (Key -> Bool) -> Int -> [Path] -> Maybe Node -> Int
traverseTill _ _ [] _ = -1
traverseTill _ _ _ Nothing = -1
traverseTill f c (p : ps) (Just n) = if f (key n) then c else traverseTill f (c + 1) ps (traverseTree p n)

path :: GenParser Char st Path
path = leftPath <|> rightPath
  where
    leftPath = do
      _ <- char 'L'
      pure L
    rightPath = do
      _ <- char 'R'
      pure R

node :: GenParser Char st RawNode
node = do
  k <- manyTill anyChar space
  _ <- char '='
  _ <- space
  _ <- char '('
  lk <- manyTill anyChar (char ',')
  _ <- space
  rk <- manyTill anyChar (char ')')
  _ <- newline
  pure (k, (lk, rk))

parseInput :: GenParser Char st ([Path], [RawNode])
parseInput = do
  p <- manyTill path newline
  _ <- newline
  rn <- manyTill node eof
  pure (p, rn)

endsWithA :: String -> Bool
endsWithA = isSuffixOf "A"

endsWithZ :: String -> Bool
endsWithZ = isSuffixOf "Z"

part1 :: ([Path], [RawNode]) -> IO Int
part1 (p, rn) = do
  let tree = buildTree "AAA" rn
  let result = traverseTill (== "ZZZ") 0 (cycle p) tree
  pure result

part2 :: ([Path], [RawNode]) -> IO Int
part2 (p, rn) = do
  let aKeys = filter endsWithA . fmap fst $ rn
  let trees = (`buildTree` rn) <$> aKeys
  let counts = traverseTill endsWithZ 0 (cycle p) <$> trees
  pure $ lcmm counts

run :: String -> IO Int
run s = do
  let res = runParser parseInput () "" s
  case res of
    Right x -> part2 x
    Left _ -> pure $ -1

lcmm :: [Int] -> Int
lcmm = foldr lcm 1

example :: IO String
example = readFile "./inputs/day8.example.txt"

input :: IO String
input = readFile "./inputs/day8.txt"
