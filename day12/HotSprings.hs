module Main where

import Control.Monad.State (State, evalState, get, modify)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar)

parseRows :: Parsec Void String [(String, [Int])]
parseRows = parseRow `sepBy1` eol
  where
    parseRow = do
      xs <- some (oneOf "#?.")
      spaceChar
      groups <- some digitChar `sepBy1` char ',' <&> map read
      return (xs, groups)

countArrangements :: String -> [Int] -> Int -> State (Map (String, [Int], Int) Int) Int
countArrangements xs groups acc = do
  mem <- get
  case Map.lookup (xs, groups, acc) mem of
    (Just res) -> return res
    Nothing -> do
      res <- go xs groups acc
      modify $ Map.insert (xs, groups, acc) res
      return res
  where
    go [] [] _ = return 1
    go [] [group] acc = return $ if acc == group then 1 else 0
    go [] groups _ = return 0
    go ('#' : xs) [] acc = return 0
    go ('#' : xs) groups@(group : _) acc =
      if group == acc
        then return 0
        else countArrangements xs groups (acc + 1)
    go ('.' : xs) groups 0 = countArrangements xs groups 0
    go ('.' : xs) [] acc = return 0
    go ('.' : xs) (group : groups) acc =
      if group == acc
        then countArrangements xs groups 0
        else return 0
    go ('?' : xs) groups acc = do
      l <- countArrangements ('#' : xs) groups acc
      r <- countArrangements ('.' : xs) groups acc
      return $ l + r

doCountArrangements :: (String, [Int]) -> Int
doCountArrangements (xs, groups) = evalState (countArrangements xs groups 0) Map.empty

unfold :: (String, [Int]) -> (String, [Int])
unfold (xs, groups) = (intercalate "?" $ replicate 5 xs, concat $ replicate 5 groups)

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  rows <- readFile "input.txt" <&> doParse parseRows
  print $ sum $ map doCountArrangements rows
  print $ sum $ map (doCountArrangements . unfold) rows