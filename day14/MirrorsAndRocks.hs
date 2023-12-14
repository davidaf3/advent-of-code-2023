module Main where

import Data.Functor ((<&>))
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, sepBy1, some)
import Text.Megaparsec.Char (eol)

parseGrid :: Parsec Void String [String]
parseGrid = some (oneOf "O#.") `sepBy1` eol <&> transpose

getLoad :: String -> Int
getLoad row = go row (length row)
  where
    go [] i = 0
    go ('O' : xs) i = i + go xs (i - 1)
    go (x : xs) i = go xs (i - 1)

tilt :: String -> String
tilt row = go row [] []
  where
    go [] rounds spaces = rounds ++ spaces
    go ('#' : xs) rounds spaces = rounds ++ spaces ++ '#' : go xs [] []
    go ('O' : xs) rounds spaces = go xs ('O' : rounds) spaces
    go ('.' : xs) rounds spaces = go xs rounds ('.' : spaces)

cycled :: [String] -> [String]
cycled grid = foldr (const (transpose . map (reverse . tilt))) grid [1 .. 4]

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

cycleTimes :: [String] -> Int -> [String]
cycleTimes grid times = go grid 0 Map.empty
  where
    go g i mem
      | i == times = g
      | otherwise =
          case Map.lookup g mem of
            (Just i') -> cycleTimes g ((times - i) `mod` (i - i'))
            Nothing -> go (cycled g) (i + 1) (Map.insert g i mem)

main :: IO ()
main = do
  grid <- readFile "input.txt" <&> doParse parseGrid
  print $ sum $ map (getLoad . tilt) grid
  print $ sum $ map getLoad $ cycleTimes grid 1000000000