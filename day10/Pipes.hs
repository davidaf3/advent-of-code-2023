{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.List (find, nub, sort)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Vector (Vector, fromList, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, sepBy1, some)
import Text.Megaparsec.Char (eol)

data Direction = N | S | E | W deriving (Show)

type Map = Vector (Vector Char)

type Point = (Int, Int)

parseMap :: Parsec Void String Map
parseMap = (some (oneOf "|-LJ7F.S") <&> fromList) `sepBy1` eol <&> fromList

getPipe :: Map -> Point -> Maybe Char
getPipe m (x, y) = m !? y >>= (!? x)

getJustPipe :: Map -> Point -> Char
getJustPipe m pt = fromMaybe (error "Pipe not found") $ getPipe m pt

getStart :: Map -> Point
getStart m =
  let points = [(x, y) | x <- [0 .. V.length (m ! 0)], y <- [0 .. V.length m]]
      start = find ((== Just 'S') . getPipe m) points
   in fromMaybe (error "Start not found") start

adjacent :: Point -> [(Point, Direction)]
adjacent (x, y) = [((x - 1, y), E), ((x + 1, y), W), ((x, y - 1), S), ((x, y + 1), N)]

getStartPipe :: Map -> Point -> Char
getStartPipe m point =
  let points = mapMaybe (\(pt, d) -> getPipe m pt <&> (pt,d,)) $ adjacent point
   in case map (\(_, d, _) -> d) $ filter (\(pt, d, p) -> isJust $ getNext pt d p) points of
        [N, S] -> '|'
        [S, N] -> '|'
        [E, W] -> '-'
        [W, E] -> '-'
        [S, W] -> 'L'
        [W, S] -> 'L'
        [S, E] -> 'J'
        [E, S] -> 'J'
        [N, E] -> '7'
        [E, N] -> '7'
        [N, W] -> 'F'
        [W, N] -> 'F'
        _ -> error "Invalid start pipe"

getStartDir :: Char -> Direction
getStartDir '|' = N
getStartDir '-' = E
getStartDir 'L' = N
getStartDir 'J' = N
getStartDir '7' = S
getStartDir 'F' = S
getStartDir _ = error "Invalid pipe"

getNext :: Point -> Direction -> Char -> Maybe (Direction, Point)
getNext (x, y) N '|' = Just (N, (x, y + 1))
getNext (x, y) S '|' = Just (S, (x, y - 1))
getNext (x, y) E '-' = Just (E, (x - 1, y))
getNext (x, y) W '-' = Just (W, (x + 1, y))
getNext (x, y) N 'L' = Just (W, (x + 1, y))
getNext (x, y) E 'L' = Just (S, (x, y - 1))
getNext (x, y) N 'J' = Just (E, (x - 1, y))
getNext (x, y) W 'J' = Just (S, (x, y - 1))
getNext (x, y) S '7' = Just (E, (x - 1, y))
getNext (x, y) W '7' = Just (N, (x, y + 1))
getNext (x, y) S 'F' = Just (W, (x + 1, y))
getNext (x, y) E 'F' = Just (N, (x, y + 1))
getNext _ _ _ = Nothing

getJustNext :: Point -> Direction -> Char -> (Direction, Point)
getJustNext pt d p = fromMaybe (error "Invalid pipe and dir") (getNext pt d p)

foldLoop :: ((Point, Direction, Char) -> a -> a) -> a -> Map -> Point -> a
foldLoop f initAcc m start =
  go initAcc $ getJustNext start (getStartDir startPipe) startPipe
  where
    startPipe = getJustPipe m start
    go acc (d, pt)
      | pt == start = f (pt, d, startPipe) acc
      | otherwise =
          let p = getJustPipe m pt
           in go (f (pt, d, p) acc) (getJustNext pt d p)

partition :: Map -> Set Point -> (Point, Direction, Char) -> (Set Point, Set Point) -> (Set Point, Set Point)
partition m loop pointData (s1, s2) =
  let (ps1, ps2) = case pointData of
        ((x, y), N, 'L') -> ([(x + 1, y - 1)], [(x - 1, y), (x - 1, y + 1), (x, y + 1)])
        ((x, y), E, 'L') -> ([(x - 1, y), (x - 1, y + 1), (x, y + 1)], [(x + 1, y - 1)])
        ((x, y), N, 'J') -> ([(x + 1, y), (x + 1, y + 1), (x, y + 1)], [(x - 1, y - 1)])
        ((x, y), W, 'J') -> ([(x - 1, y - 1)], [(x + 1, y), (x + 1, y + 1), (x, y + 1)])
        ((x, y), S, '7') -> ([(x - 1, y + 1)], [(x + 1, y), (x + 1, y - 1), (x, y - 1)])
        ((x, y), W, '7') -> ([(x + 1, y), (x + 1, y - 1), (x, y - 1)], [(x - 1, y + 1)])
        ((x, y), S, 'F') -> ([(x - 1, y), (x - 1, y - 1), (x, y - 1)], [(x + 1, y + 1)])
        ((x, y), E, 'F') -> ([(x + 1, y + 1)], [(x - 1, y), (x - 1, y - 1), (x, y - 1)])
        _ -> ([], [])
   in (foldr Set.insert s1 (filter isValid ps1), foldr Set.insert s2 (filter isValid ps2))
  where
    isValid pt = not (Set.member pt loop) && isJust (getPipe m pt)

expand :: Map -> Set Point -> Set Point -> Set Point -> Set Point
expand m loop pts set =
  let nextSet = set `Set.union` pts
      adjacents = Set.fromList $ concatMap (map fst . adjacent) pts
      next = Set.filter (isJust . getPipe m) $ adjacents \\ loop \\ nextSet
   in if null next then nextSet else expand m loop next nextSet

isEdge :: Map -> Point -> Bool
isEdge m (x, y) = x == 0 || y == 0 || x == length (m ! 0) - 1 || x == length m - 1

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  m <- readFile "input.txt" <&> doParse parseMap
  let start@(startX, startY) = getStart m
  let startPipe = getStartPipe m start
  let m' = m // [(startY, (m ! startY) // [(startX, startPipe)])]
  let loop = foldLoop (\(pt, _, _) s -> Set.insert pt s) Set.empty m' start
  print $ flip div 2 $ Set.size loop
  let (part1, part2) = foldLoop (partition m' loop) (Set.empty, Set.empty) m' start
  let (part1', part2') = (expand m' loop part1 Set.empty, expand m' loop part2 Set.empty)
  let inside = if any (isEdge m') part1' then part2' else part1'
  print $ Set.size inside