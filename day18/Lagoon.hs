module Main where

import Data.Functor ((<&>))
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Numeric (readHex)
import Text.Megaparsec (Parsec, count, errorBundlePretty, oneOf, parse, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, eol, hexDigitChar, spaceChar, string)

type Point = (Int, Int)

data Move = Move Char Int deriving (Show)

parseMoves :: Parsec Void String [Move]
parseMoves = parseMove `sepBy1` eol
  where
    parseMove = do
      dir <- oneOf "UDLR"
      some spaceChar
      steps <- some digitChar <&> read
      some spaceChar >> string "(#" >> count 6 hexDigitChar >> char ')'
      return $ Move dir steps

parseMoves' :: Parsec Void String [Move]
parseMoves' = parseMove `sepBy1` eol
  where
    parseMove = do
      oneOf "UDLR" >> spaceChar >> some digitChar >> some spaceChar >> string "(#"
      steps <- count 5 hexDigitChar <&> fst . head . readHex
      hexDir <- hexDigitChar
      char ')'
      let dir = case hexDir of
            '0' -> 'R'
            '1' -> 'D'
            '2' -> 'L'
            '3' -> 'U'
      return $ Move dir steps

getCorners :: [Move] -> Point -> Char -> [(Point, Char)]
getCorners [] _ _ = []
getCorners ((Move dir steps) : rest) cur@(x, y) curDir =
  let next = case dir of
        'U' -> (x, y - steps)
        'D' -> (x, y + steps)
        'L' -> (x - steps, y)
        'R' -> (x + steps, y)
   in (cur, if dir == 'L' || dir == 'R' then dir else curDir) : getCorners rest next dir

getIntersections :: [(Point, Char)] -> [(Int, [Int])]
getIntersections corners = go (sort corners) [] []
  where
    go :: [(Point, Char)] -> [(Int, Char)] -> [(Int, Char)] -> [(Int, [Int])]
    go [(p@(x, _), _)] _ _ = [(x, [])]
    go (((x1, y1), ct1) : p2@((x2, _), _) : ps) colCorners colIntersections
      | x1 == x2 = go (p2 : ps) ((y1, ct1) : colCorners) colIntersections
      | otherwise =
          let colCorners' = (y1, ct1) : colCorners
              cuttingCandidates = colCorners' ++ colIntersections
              nextColIntersections = getNextColIntersections cuttingCandidates
              colCorners'' = filterCutting (sort $ nub cuttingCandidates) (Set.fromList colCorners') 0
           in if x2 == x1 + 1
                then (x1, colCorners'') : go (p2 : ps) [] nextColIntersections
                else (x1, colCorners'') : (x1 + 1, map fst nextColIntersections) : go (p2 : ps) [] nextColIntersections
    getNextColIntersections ys =
      let yCount = foldr (Map.alter (Just . (+ 1) . fromMaybe 0)) Map.empty ys
       in sort $ Map.keys $ Map.filter (== 1) yCount
    filterCutting [] _ _ = []
    filterCutting (c@(y, dir) : cs) cornersSet lefts =
      if not $ Set.member c cornersSet
        then y : filterCutting cs cornersSet (if dir == 'L' then lefts + 1 else lefts - 1)
        else
          let lefts' = if dir == 'L' then lefts + 0.5 else lefts - 0.5
           in if lefts' == 0 || lefts == 0
                then y : filterCutting cs cornersSet lefts'
                else filterCutting cs cornersSet lefts'

getArea :: [(Point, Char)] -> Int
getArea corners = go (getIntersections corners) 0
  where
    go [_] lastArea = lastArea
    go (c1@(x1, ys) : c2@(x2, _) : cs) lastArea =
      let ysArea = colArea ys
       in (x2 - x1) * ysArea + go (c2 : cs) ysArea
    colArea [] = 0
    colArea (y1 : y2 : ys) = (y2 - y1 + 1) + colArea ys

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  file <- readFile "input.txt"
  let moves = doParse parseMoves file
      (Move initDir _) = last moves
      corners = sort $ getCorners moves (0, 0) initDir
  print $ getArea corners
  let moves' = doParse parseMoves' file
      (Move initDir' _) = last moves'
      corners' = sort $ getCorners moves' (0, 0) initDir'
  print $ getArea corners'
