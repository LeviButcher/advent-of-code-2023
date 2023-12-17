module Day16
  ( example,
    Board (..),
    prettyPrint,
    boardMax,
    Part (..),
    Beam (..),
    EState (..),
    setSquare,
    emptyBoard,
    getSquare,
    parseBoard,
    part1,
    nextPos,
    inBounds,
    part2,
  )
where

import Data.Either (fromRight, rights)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Vector (Vector, fromList, generate, replicate, toList, unsafeUpd, (!))
import Debug.Trace (traceShow)

-- Assume 0,0 is min
newtype Board t = Board (Vector (Vector t))

data BoardError = OutOfBounds | Cycle
  deriving (Show, Eq)

instance (Show t) => Show (Board t) where
  show (Board x) = unlines . toList $ rowS
    where
      rowS = mconcat . toList . fmap show <$> x

prettyPrint :: Board Char -> IO ()
prettyPrint (Board x) = do
  putStr $ unlines . toList . fmap toList $ x

count :: (Eq a) => a -> Board a -> Int
count x (Board b) = sum . toList $ rowCount
  where
    rowCount = length . filter (== x) . toList <$> b

-- (Row, Col)
type Pos = (Int, Int)

type Max = (Int, Int)

emptyBoard :: Max -> t -> Board t
emptyBoard (r, c) t = Board row
  where
    col = Data.Vector.replicate c t
    row = Data.Vector.replicate r col

boardMax :: Board t -> Max
boardMax (Board x) = (rL, cL)
  where
    rL = length x
    cL = maximum . fmap length $ x

setSquare :: Pos -> t -> Board t -> Either BoardError (Board t)
setSquare p@(r, c) t b@(Board x) = if inBounds mx p then Right newBoard else Left OutOfBounds
  where
    mx = boardMax b
    rowVector = unsafeUpd (x ! r) [(c, t)]
    newBoard = Board $ unsafeUpd x [(r, rowVector)]

getSquare :: Pos -> Board t -> Either BoardError t
getSquare p@(r, c) b@(Board x) = if inBounds mx p then Right value else Left OutOfBounds
  where
    mx = boardMax b
    rowVector = x ! r
    value = rowVector ! c

-- LMirror = /
-- RMirror = \
-- VSpliiter = |
-- HSpliiter = |
data Part = Empty | LMirror | RMirror | VSplitter | HSplitter
  deriving (Show, Eq)

charToPart :: Char -> Part
charToPart '|' = VSplitter
charToPart '/' = LMirror
charToPart '\\' = RMirror
charToPart '-' = HSplitter
charToPart _ = Empty

partString :: String -> [Part]
partString = map charToPart

data Beam = N | S | E | W
  deriving (Show, Eq)

data EState = Hot | Cold
  deriving (Show, Eq)

type EnergizedBoard = Board EState

type PartBoard = Board Part

inBounds :: Max -> Pos -> Bool
inBounds (mr, my) (x, y) = x < mr && y < my && x >= 0 && y >= 0

moveWithBeam :: Pos -> Beam -> Pos
moveWithBeam (r, c) E = (r, c + 1)
moveWithBeam (r, c) W = (r, c - 1)
moveWithBeam (r, c) N = (r - 1, c)
moveWithBeam (r, c) S = (r + 1, c)

-- LMirror '/'
-- RMirror '\'

nextPos :: Pos -> Beam -> Part -> [(Beam, Pos)]
nextPos p b Empty = pure (b, moveWithBeam p b)
nextPos p N HSplitter = [(E, moveWithBeam p E), (W, moveWithBeam p W)]
nextPos p S HSplitter = [(E, moveWithBeam p E), (W, moveWithBeam p W)]
nextPos p b HSplitter = pure (b, moveWithBeam p b)
nextPos p E VSplitter = [(N, moveWithBeam p N), (S, moveWithBeam p S)]
nextPos p W VSplitter = [(N, moveWithBeam p N), (S, moveWithBeam p S)]
nextPos p b VSplitter = pure (b, moveWithBeam p b)
nextPos p E LMirror = pure (N, moveWithBeam p N)
nextPos p W LMirror = pure (S, moveWithBeam p S)
nextPos p N LMirror = pure (E, moveWithBeam p E)
nextPos p S LMirror = pure (W, moveWithBeam p W)
nextPos p E RMirror = pure (S, moveWithBeam p S)
nextPos p W RMirror = pure (N, moveWithBeam p N)
nextPos p N RMirror = pure (W, moveWithBeam p W)
nextPos p S RMirror = pure (E, moveWithBeam p E)

type HistoryList = [(Pos, Beam)]

fireBeam :: Pos -> Beam -> HistoryList -> PartBoard -> EnergizedBoard -> Either BoardError (HistoryList, EnergizedBoard)
fireBeam p b hl pb eb = do
  let mx = boardMax pb
  let isValidPos = inBounds mx p
  if isValidPos then Right () else Left OutOfBounds
  let isPriorPath = isJust $ find (== (p, b)) hl
  if not isPriorPath then Right () else Left Cycle
  newEB <- setSquare p Hot eb
  currPart <- getSquare p pb
  let newH = (p, b) : hl
  let next = nextPos p b currPart
  let finalEB = foldl (\(currHL, currEB) (be, pp) -> (currHL, currEB) `fromRight` fireBeam pp be currHL pb currEB) (newH, newEB) next
  Right finalEB

-- Ignore invalid strings
parseBoard :: String -> Board Char
parseBoard s = Board $ fromList rows
  where
    rows = fromList <$> lines s

parsePartBoard :: String -> Board Part
parsePartBoard s = Board $ fromList rows
  where
    rows = fromList . partString <$> lines s

simpleEBoard :: EnergizedBoard -> Board Char
simpleEBoard (Board e) = Board rows
  where
    rows = fmap (fmap (\x -> if x == Hot then '#' else '.')) e

generateBeamStarts :: Max -> [(Pos, Beam)]
generateBeamStarts (r, c) = topB <> bottomB <> leftB <> rightB
  where
    cols = [0 .. c]
    rows = [0 .. r]
    topB = zipWith (\a b -> ((0, a), b)) cols (Prelude.replicate c S)
    bottomB = zipWith (\a b -> ((r - 1, a), b)) cols (Prelude.replicate c N)
    leftB = zipWith (\a b -> ((a, 0), b)) rows (Prelude.replicate r E)
    rightB = zipWith (\a b -> ((a, c - 1), b)) rows (Prelude.replicate r W)

part1 :: IO ()
part1 = do
  input <- readFile "inputs/day16.txt"
  let pBoard = parsePartBoard input
  let history = []
  let mx = boardMax pBoard
  let eBoard = emptyBoard mx Cold
  let result = fireBeam (0, 0) E history pBoard eBoard
  let (_, finalBoard) = (history, eBoard) `fromRight` result
  let c = count Hot finalBoard
  print c

part2 :: IO ()
part2 = do
  input <- readFile "inputs/day16.txt"
  let pBoard = parsePartBoard input
  let history = []
  let mx = boardMax pBoard
  let eBoard = emptyBoard mx Cold
  let beams = generateBeamStarts mx
  let results = rights $ (\(p, b) -> fireBeam p b history pBoard eBoard) <$> beams
  let c = maximum $ count Hot . snd <$> results
  print c

-- let (_, finalBoard) = (history, eBoard) `fromRight` result
-- let c = count Hot finalBoard
-- print c

-- needed Double slash to escape slash
example :: [String]
example =
  [ ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|...."
  ]
