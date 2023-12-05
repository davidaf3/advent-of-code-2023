module Main where

import Data.Functor (($>), (<&>))
import Data.List (sort)
import Debug.Trace (trace)
import Text.Parsec
  ( Parsec,
    char,
    digit,
    endBy,
    endOfLine,
    eof,
    letter,
    many1,
    manyTill,
    parse,
    sepBy1,
    space,
    string,
    (<|>),
  )

type Range = (Int, Int)

data MapLine = MapLine Range Range deriving (Show)

type Map = [MapLine]

instance Eq MapLine where
  (==) (MapLine _ (s1, _)) (MapLine _ (s2, _)) = s1 == s2

instance Ord MapLine where
  compare (MapLine _ (s1, _)) (MapLine _ (s2, _))
    | s1 > s2 = GT
    | s1 < s2 = LT
    | otherwise = EQ

parseMapLine :: Parsec String () MapLine
parseMapLine = do
  line <- sepBy1 (many1 digit) (char ' ') <&> map read
  case line of
    [dst, src, len] -> return $ MapLine (dst, dst + len - 1) (src, src + len - 1)
    n -> trace (show n) error "Parse error"

parseMap :: Parsec String () Map
parseMap = do
  manyTill (letter <|> char '-' <|> char ':' <|> char ' ') endOfLine
  endBy parseMapLine ((endOfLine $> ()) <|> eof) <&> sort

parseAlmanac :: Parsec String () ([Int], [Map])
parseAlmanac = do
  seeds <- string "seeds: " >> sepBy1 (many1 digit) (char ' ') <&> map read
  endOfLine >> endOfLine
  maps <- sepBy1 parseMap endOfLine
  return (seeds, maps)

applyMapSeed :: Map -> Int -> Int
applyMapSeed aMap src =
  let matches = filter (\(MapLine _ (s, e)) -> src >= s && src <= e) aMap
   in case matches of
        [MapLine (ds, de) (s, e)] -> ds + (src - s)
        [] -> src
        _ -> error "No more than one line should match"

applyMapRange :: Map -> Range -> [Range]
applyMapRange [] r = [r]
applyMapRange ((MapLine (ds, de) (ms, me)) : rst) r@(s, e)
  | e < ms = [r]
  | s > me = applyMapRange rst r
  | s >= ms && e <= me = [(ds + (s - ms), de - (me - e))]
  | s < ms && e >= ms && s <= me = [(s, ms - 1), (ds, de - (me - e))]
  | s >= ms && s <= me && e > me = (ds + (s - ms), de) : applyMapRange rst (me + 1, e)
  | s < ms && e > me = (s, ms - 1) : (ds, de) : applyMapRange rst (me + 1, e)
  | otherwise = error "Should not reach here"

applyMapRanges :: Map -> [Range] -> [Range]
applyMapRanges aMap = concatMap (applyMapRange aMap)

getLocations :: [Int] -> [Map] -> [Int]
getLocations seeds maps = map getLocation seeds
  where
    getLocation seed = foldr applyMapSeed seed (reverse maps)

getLocationsRanges :: [Range] -> [Map] -> [Range]
getLocationsRanges ranges maps = foldr (concatMap . applyMapRange) ranges $ reverse maps

asRanges :: [Int] -> [Range]
asRanges [] = []
asRanges (s : len : rst) = (s, s + len - 1) : asRanges rst
asRanges _ = error "Odd number of seeds"

main :: IO ()
main = do
  (seeds, maps) <- readFile "input.txt" <&> either (error . show) id . parse parseAlmanac ""
  print $ minimum $ getLocations seeds maps
  print $ minimum $ map fst $ getLocationsRanges (asRanges seeds) maps
