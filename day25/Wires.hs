{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import System.Random.Stateful (StatefulGen, mkStdGen, runStateGen_, uniformRM)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, some)
import Text.Megaparsec.Char (char, eol, letterChar, string)

type Graph = Map [String] (Set [String])

parseWiring :: Parsec Void String [(String, String)]
parseWiring = do
  src <- some letterChar
  string ": "
  dst <- some letterChar `sepBy1` char ' '
  return (map (src,) dst ++ map (,src) dst)

mkGraph :: [(String, String)] -> Graph
mkGraph = foldr (\(s, d) -> Map.alter (Just . Set.insert [d] . fromMaybe Set.empty) [s]) Map.empty

contract :: (StatefulGen g m) => Graph -> Int -> g -> m Graph
contract graph t gen
  | Map.size graph == t = return graph
  | otherwise = do
      (src, dst) <- randomEdge gen graph
      let srcNeighbours = Set.delete dst $ fromJust $ Map.lookup src graph
          dstNeighbours = Set.delete src $ fromJust $ Map.lookup dst graph
          new = src ++ dst
          graph' = Map.insert new (Set.union srcNeighbours dstNeighbours) graph
          graph'' = Map.delete src $ Map.delete dst graph'
          replaceNeighbours = Just . replace dst new . replace src new . fromJust
          graph''' = foldr (Map.alter replaceNeighbours) graph'' (Map.keys graph'')
      contract graph''' t gen
  where
    replaceNeighbours src dst new = Just . replace dst new . replace src new . fromJust

replace :: (Ord a) => a -> a -> Set a -> Set a
replace old new set =
  if Set.member old set
    then Set.insert new $ Set.delete old set
    else set

randomEdge :: (StatefulGen g m) => g -> Graph -> m ([String], [String])
randomEdge gen graph = do
  i <- uniformRM (0, Map.size graph - 1) gen
  let key = Map.keys graph !! i
      neighbours = fromJust $ Map.lookup key graph
  j <- uniformRM (0, Set.size neighbours - 1) gen
  return (key, Set.elems neighbours !! j)

cutSize :: Graph -> Graph -> Int
cutSize original graph =
  let [s, t] = Map.keys graph
      targetSet = Set.fromList $ map (: []) t
      countEdges = Set.size . Set.intersection targetSet
   in sum $ map (\k -> countEdges $ fromJust $ Map.lookup [k] original) s

fastMinCut :: (StatefulGen g m) => Graph -> Graph -> g -> m Graph
fastMinCut original graph gen =
  let nodeCount = Map.size graph
   in if nodeCount <= 6
        then contract graph 2 gen
        else do
          let t = round (1 + fromIntegral nodeCount / 2)
          g1 <- contract graph t gen
          g2 <- contract graph t gen
          g1' <- fastMinCut original g1 gen
          g2' <- fastMinCut original g2 gen
          return $
            if cutSize original g1' < cutSize original g2'
              then g1'
              else g2'

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  edges <- readFile "input.txt" <&> concat . doParse (parseWiring `sepBy1` eol)
  let graph = mkGraph edges
      minCut = runStateGen_ (mkStdGen 137) (fastMinCut graph graph)
      [s, t] = Map.keys minCut
  print $ length s * length t