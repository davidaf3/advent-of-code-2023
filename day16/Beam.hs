module Main where

import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

type Point = (Int, Int)

type BeamState = (Point, (Int, Int))

type Grid = Vector (Vector Char)

getTile :: Grid -> Point -> Maybe Char
getTile v (x, y) = v !? y >>= (!? x)

getNextState :: Grid -> BeamState -> [BeamState]
getNextState grid (p@(x, y), spd@(spdX, spdY)) =
  let spds' = case getTile grid p of
        (Just '.') -> [spd]
        (Just '/') -> [(-spdY, -spdX)]
        (Just '\\') -> [(spdY, spdX)]
        (Just '-') -> [(1, 0), (-1, 0)]
        (Just '|') -> [(0, 1), (0, -1)]
        _ -> error "Invalid tile"
   in map (\spd'@(spdX', spdY') -> ((x + spdX', y + spdY'), spd')) spds'

getEnergized :: Grid -> BeamState -> Set Point
getEnergized grid beam = go grid [beam] Set.empty Set.empty
  where
    go grid beams visited ps =
      let stuck b@(p, _) = Set.member b visited || isNothing (getTile grid p)
          beams' = filter (not . stuck) beams
          visited' = foldr Set.insert visited beams'
          ps' = foldr (Set.insert . fst) ps beams'
       in if null beams'
            then ps
            else go grid (concatMap (getNextState grid) beams') visited' ps'

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> V.fromList . map V.fromList . lines
  print $ Set.size $ getEnergized grid ((0, 0), (1, 0))
  let gridSize = V.length grid - 1
      beams =
        concat
          [ [ ((x, 0), (0, 1)),
              ((x, gridSize), (0, -1)),
              ((0, x), (1, 0)),
              ((gridSize, x), (-1, 0))
            ]
            | x <- [0 .. gridSize]
          ]
  print $ maximum $ map (Set.size . getEnergized grid) beams