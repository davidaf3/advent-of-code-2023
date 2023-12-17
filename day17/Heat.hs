module Main where

import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Maybe (mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as Set
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type Grid = Vector (Vector Int)

type Point = (Int, Int)

type State = (Point, (Int, Int), Int)

type NextSpds = Int -> (Int, Int) -> [(Int, Int)]

data WithCost a = WithCost Int a

instance Eq (WithCost a) where
  (==) (WithCost c1 _) (WithCost c2 _) = c1 == c2

instance Ord (WithCost a) where
  compare (WithCost c1 _) (WithCost c2 _) = compare c1 c2

parseGrid :: [String] -> Grid
parseGrid = V.fromList . map (V.fromList . map (read . singleton))

getHeat :: Grid -> Point -> Maybe Int
getHeat grid (x, y) = grid !? y >>= (!? x)

manhattanDist :: Point -> State -> Int
manhattanDist (gx, gy) ((x, y), _, _) = abs (x - gx) + abs (y - gy)

isFinalCrucible :: Point -> State -> Bool
isFinalCrucible goal (p, _, _) = goal == p

expandCrucible :: NextSpds -> Grid -> WithCost State -> [WithCost State]
expandCrucible nextSpds grid (WithCost cost ((x, y), spd, sameDir)) =
  mapMaybe next $ nextSpds sameDir spd
  where
    next spd'@(spdX', spdY') =
      let p' = (x + spdX', y + spdY')
          sameDir' = if spd' == spd then sameDir + 1 else 0
       in getHeat grid p' <&> (\heat -> WithCost (cost + heat) (p', spd', sameDir'))

getNextSpds :: NextSpds
getNextSpds sameDir spd@(spdX, spdY) =
  (spdY, spdX) : (-spdY, -spdX) : [spd | sameDir < 2]

getNextSpdsUltra :: NextSpds
getNextSpdsUltra sameDir spd@(spdX, spdY) =
  [x | sameDir >= 3, x <- [(spdY, spdX), (-spdY, -spdX)]] ++ [spd | sameDir < 9]

astar ::
  (Ord a) =>
  (a -> Bool) ->
  (a -> Int) ->
  (WithCost a -> [WithCost a]) ->
  [WithCost a] ->
  WithCost a
astar isFinal h expand init =
  let initQueue = foldr PQ.insert PQ.empty init
   in go initQueue Set.empty
  where
    go queue visited =
      let (wc@(WithCost _ state), queue') = PQ.deleteFindMin queue
       in if Set.member state visited
            then go queue' visited
            else
              if isFinal state
                then wc
                else
                  let visited' = Set.insert state visited
                      queue'' = foldr PQ.insert queue' $ expand wc
                   in go queue'' visited'

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> parseGrid . lines
  let initStates =
        [ WithCost 0 ((0, 0), (1, 0), 0),
          WithCost 0 ((0, 0), (0, 1), 0)
        ]
      rows = V.length grid
      cols = V.length (grid ! 0)
      goal = (cols - 1, rows - 1)
      isFinal = isFinalCrucible goal
      h = manhattanDist goal
      expand = expandCrucible getNextSpds grid
      expandUltra = expandCrucible getNextSpdsUltra grid
      (WithCost cost _) = astar isFinal h expand initStates
      (WithCost cost' _) = astar isFinal h expandUltra initStates
  print cost
  print cost'