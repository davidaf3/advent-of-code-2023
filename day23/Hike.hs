{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type Point = (Int, Int)

type Grid = Vector (Vector Char)

getTile :: Grid -> Point -> Maybe Char
getTile grid (x, y) = grid !? y >>= (!? x)

isValidPoint :: Grid -> Point -> Bool
isValidPoint grid p =
  let tile = getTile grid p in isJust tile && tile /= Just '#'

expandHike :: Maybe Char -> Point -> [Point]
expandHike (Just '.') (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
expandHike (Just '>') (x, y) = [(x + 1, y)]
expandHike (Just '<') (x, y) = [(x - 1, y)]
expandHike (Just '^') (x, y) = [(x, y - 1)]
expandHike (Just 'v') (x, y) = [(x, y + 1)]
expandHike _ _ = error "Invalid tile"

condense :: Grid -> Bool -> Point -> Map Point (Map Point Int)
condense grid climb goal = go (Set.singleton ((1, 0), (1, 0))) [((1, 0), (1, 1), 1)] Map.empty
  where
    go visited [] graph = graph
    go visited ((origin, p, len) : ps) graph =
      let visited' = Set.insert (origin, p) visited
       in if p == goal
            then go visited' ps (addEdge origin p len graph)
            else case getChildren visited' origin p of
              [] -> go visited' ps graph
              [child] -> go visited' ((origin, child, len + 1) : ps) graph
              children -> go visited' (map (p,,1) children ++ ps) (addEdge origin p len graph)
    getChildren visited origin p =
      let children = expandHike (getTile grid p) p
       in filter (\c -> isValidPoint grid c && Set.notMember (origin, c) visited) children
    addEdge origin p len graph =
      let graph' = Map.alter (Just . Map.insert p len . fromMaybe Map.empty) origin graph
       in if climb
            then Map.alter (Just . Map.insert origin len . fromMaybe Map.empty) p graph'
            else graph'

longestHike :: Map Point (Map Point Int) -> Point -> Point -> Int
longestHike graph p goal = go Set.empty p
  where
    go visited p
      | p == goal = 0
      | otherwise =
          let visited' = Set.insert p visited
              children = maybe [] Map.toList (Map.lookup p graph)
              childrenFiltered = filter ((`Set.notMember` visited') . fst) children
           in if null childrenFiltered
                then minBound
                else maximum (map (\(c, len) -> len + go visited' c) childrenFiltered)

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> V.fromList . map V.fromList . lines
  let goal = (V.length (grid ! 0) - 2, V.length grid - 1)
      graph = condense grid False goal
      graph' = condense grid True goal
  print $ longestHike graph (1, 0) goal
  print $ longestHike graph' (1, 0) goal