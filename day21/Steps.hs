module Main where

import Data.Functor ((<&>))
import Data.List (find, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type Grid = Vector (Vector Char)

type Point = (Int, Int)

getTile :: Grid -> Point -> Maybe Char
getTile grid (x, y) = grid !? y >>= (!? x)

getAllIndexes :: Grid -> [Point]
getAllIndexes grid = [(x, y) | x <- [0 .. V.length (grid ! 0)], y <- [0 .. V.length grid]]

isValid :: Maybe Char -> Bool
isValid tile = tile == Just '.' || tile == Just 'S'

fillSteps :: Grid -> Point -> Int -> Int
fillSteps grid start = go [start] Map.empty
  where
    modPoint (x, y) = (x `mod` V.length (grid ! 0), y `mod` V.length grid)
    go ps visited 0 = length ps + Map.size (Map.filter id visited)
    go ps visited n =
      let visited' = foldr (`Map.insert` even n) visited ps
          ps' = concatMap (\(x, y) -> [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]) ps
          ps'' = nub $ filter (\p -> Map.notMember p visited' && isValid (getTile grid $ modPoint p)) ps'
       in go ps'' visited' (n - 1)

lagrange :: [(Int, Int)] -> Int -> Int
lagrange points x = sum [yi * l xi | (xi, yi) <- points]
  where
    l xi = product [(x - xj) `div` (xi - xj) | (xj, _) <- points, xj /= xi]

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> V.fromList . map V.fromList . lines
  let len = V.length grid
      halfLen = len `div` 2
      start = fromMaybe (error "No start") $ find ((Just 'S' ==) . getTile grid) $ getAllIndexes grid
      points = map (fillSteps grid start) [halfLen, halfLen + len, halfLen + 2 * len]
  print $ fillSteps grid start 64
  print $ lagrange (zip [0 ..] points) ((26501365 - halfLen) `div` len)