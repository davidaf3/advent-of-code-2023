module Main where

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type Point = (Int, Int)

type Grid = Vector (Vector Char)

type ExpandFn = Maybe Char -> Point -> [Point]

getTile :: Grid -> Point -> Maybe Char
getTile grid (x, y) = grid !? y >>= (!? x)

isValidPoint :: Grid -> Point -> Bool
isValidPoint grid p =
  let tile = getTile grid p in isJust tile && tile /= Just '#'

getChildren :: Grid -> ExpandFn -> Set Point -> Point -> [Point]
getChildren grid expandFn visited p =
  let children = expandFn (getTile grid p) p
   in filter (\c -> isValidPoint grid c && Set.notMember c visited) children

expandHike :: ExpandFn
expandHike (Just '.') (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
expandHike (Just '>') (x, y) = [(x + 1, y)]
expandHike (Just '<') (x, y) = [(x - 1, y)]
expandHike (Just '^') (x, y) = [(x, y - 1)]
expandHike (Just 'v') (x, y) = [(x, y + 1)]
expandHike _ _ = error "Invalid tile"

expandHikeClimbing :: ExpandFn
expandHikeClimbing (Just '.') (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
expandHikeClimbing (Just '>') (x, y) = [(x + 1, y), (x - 1, y)]
expandHikeClimbing (Just '<') (x, y) = [(x - 1, y), (x + 1, y)]
expandHikeClimbing (Just '^') (x, y) = [(x, y - 1), (x, y + 1)]
expandHikeClimbing (Just 'v') (x, y) = [(x, y + 1), (x, y - 1)]
expandHikeClimbing _ _ = error "Invalid tile"

longestHike :: Grid -> ExpandFn -> Set Point -> Point -> Point -> Int
longestHike grid expandFn visited p goal = go visited p
  where
    go visited p
      | p == goal = 0
      | otherwise =
          let visited' = Set.insert p visited
              children = getChildren grid expandFn visited' p
           in if null children
                then minBound
                else 1 + maximum (map (go visited') children)

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> V.fromList . map V.fromList . lines
  let goal = (V.length (grid ! 0) - 2, V.length grid - 1)
  print $ longestHike grid expandHike Set.empty (1, 0) goal
  print $ longestHike grid expandHikeClimbing Set.empty (1, 0) goal