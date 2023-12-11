{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, some)

type Point = (Int, Int)

parseRow :: Int -> Parsec Void String [Point]
parseRow y = do
  cs <- some (oneOf "#.")
  return $ foldr addPoint [] $ zip [0 ..] cs
  where
    addPoint (x, '#') ps = (x, y) : ps
    addPoint _ ps = ps

parseGrid :: [String] -> [Point]
parseGrid ls = concatMap (\(y, l) -> doParse (parseRow y) l) $ zip [0 ..] ls

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

getExpandedBy :: Set Int -> [Int] -> Int -> [Int]
getExpandedBy _ [] _ = []
getExpandedBy set (x : xs) acc =
  let newAcc = if Set.member x set then acc else acc + 1
   in newAcc : getExpandedBy set xs newAcc

expand :: V.Vector Int -> V.Vector Int -> Int -> Point -> Point
expand expandedByX expandedByY mult (x, y) =
  (x + (expandedByX ! x) * mult, y + (expandedByY ! y) * mult)

pairs :: [a] -> [(a, a)]
pairs [_] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  galaxies <- readFile "input.txt" <&> parseGrid . lines
  let maxX = maximum $ map fst galaxies
      maxY = maximum $ map snd galaxies
      nonEmptyXs = Set.fromList $ map fst galaxies
      nonEmptyYs = Set.fromList $ map snd galaxies
      expandedByX = V.fromList $ getExpandedBy nonEmptyXs [0 .. maxX] 0
      expandedByY = V.fromList $ getExpandedBy nonEmptyYs [0 .. maxY] 0
      galaxies' = map (expand expandedByX expandedByY 1) galaxies
      galaxies'' = map (expand expandedByX expandedByY 999999) galaxies
  print $ sum $ map (uncurry dist) $ pairs galaxies'
  print $ sum $ map (uncurry dist) $ pairs galaxies''