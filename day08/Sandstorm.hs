module Main where

import Control.Arrow (ArrowChoice (right))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar, upperChar)

parseNode :: Parsec Void String (String, (String, String))
parseNode = do
  name <- some (upperChar <|> digitChar)
  spaceChar >> char '=' >> spaceChar >> char '('
  left <- some (upperChar <|> digitChar)
  char ',' >> spaceChar
  right <- some (upperChar <|> digitChar)
  char ')'
  return (reverse name, (reverse left, reverse right))

parseMap :: Parsec Void String (String, Map String (String, String))
parseMap = do
  directions <- some (oneOf "RL")
  eol >> eol
  nodes <- parseNode `sepBy1` eol
  return (directions, Map.fromList nodes)

nextNode :: Char -> Map String (String, String) -> String -> String
nextNode d theMap current = case Map.lookup current theMap of
  (Just (l, r)) -> if d == 'L' then l else r
  _ -> error $ "Node not found: " ++ current

followMap :: String -> Map String (String, String) -> String -> Int
followMap _ _ "ZZZ" = 0
followMap (d : ds) theMap node = 1 + followMap ds theMap (nextNode d theMap node)

followMapTillZ :: String -> Map String (String, String) -> String -> Int
followMapTillZ (d : ds) theMap node
  | head node == 'Z' = 0
  | otherwise = 1 + followMapTillZ ds theMap (nextNode d theMap node)

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  (directions, theMap) <- readFile "input.txt" <&> doParse parseMap
  print $ followMap (cycle directions) theMap "AAA"
  let startingNodes = filter ((== 'A') . head) $ Map.keys theMap
  print $ foldl' lcm 1 $ map (followMapTillZ (cycle directions) theMap) startingNodes