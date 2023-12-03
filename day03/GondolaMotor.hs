{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)

type Point = (Int, Int)

type Symbol = (Point, Char)

type Symbols = Map Point Char

data PartNumber = PartNumber
  { number :: Int,
    nLen :: Int,
    nPos :: Point
  }
  deriving (Show)

type Schematic = ([PartNumber], Symbols)

parseNumber :: String -> Int -> Int -> (PartNumber, String)
parseNumber str x y = go "" 0 str
  where
    go n len str
      | null str || not (isDigit $ head str) =
          (PartNumber (read $ reverse n) len (x, y), str)
      | otherwise = go (head str : n) (len + 1) (tail str)

parseRow :: Schematic -> Int -> Int -> String -> Schematic
parseRow parsed _ _ "" = parsed
parseRow parsed@(ns, ss) x y row@(c : cs)
  | isDigit c =
      let (n, rst) = parseNumber row x y
       in parseRow (n : ns, ss) (x + nLen n) y rst
  | c /= '.' = parseRow (ns, Map.insert (x, y) c ss) (x + 1) y cs
  | otherwise = parseRow parsed (x + 1) y cs

parseSchematic :: [String] -> Schematic
parseSchematic schematic =
  let (ns, ss, _) = foldl doParseRow ([], Map.empty, 0) schematic
   in (ns, ss)
  where
    doParseRow (ns, ss, y) row =
      let (newNs, newSs) = parseRow (ns, ss) 0 y row
       in (newNs, newSs, y + 1)

withAdjacentSymbols :: Symbols -> PartNumber -> (PartNumber, [Symbol])
withAdjacentSymbols ss n@(PartNumber _ len (nx, ny)) =
  let positions = [(x, y) | x <- [nx - 1 .. nx + len], y <- [ny - 1, ny + 1]]
      positions' = (nx - 1, ny) : (nx + len, ny) : positions
      adjacentSymbols = mapMaybe (\pos -> Map.lookup pos ss <&> (pos,)) positions'
   in (n, adjacentSymbols)

getSymbolsMap :: [(PartNumber, [Symbol])] -> Map Point [Int]
getSymbolsMap partNumbers =
  let symbolsWithNumbers = concatMap explodeNumberAndSymbols partNumbers
   in foldr addSymbolNumber Map.empty symbolsWithNumbers
  where
    explodeNumberAndSymbols (n, ss) = map ((,number n) . fst) ss
    addSymbolNumber (sPos, n) = Map.alter (Just . (n :) . fromMaybe []) sPos

getGearRatio :: Map Point [Int] -> Symbol -> Maybe Int
getGearRatio map (sPos, '*') = case Map.lookup sPos map of
  (Just [n1, n2]) -> Just $ n1 * n2
  _ -> Nothing
getGearRatio _ _ = Nothing

main :: IO ()
main = do
  (ns, ss) <- readFile "input.txt" <&> parseSchematic . lines
  let partNumbers = filter ((not . null) . snd) $ map (withAdjacentSymbols ss) ns
  print $ sum $ map (number . fst) partNumbers
  let symbolsMap = getSymbolsMap partNumbers
  print $ sum $ mapMaybe (getGearRatio symbolsMap) $ Map.toList ss