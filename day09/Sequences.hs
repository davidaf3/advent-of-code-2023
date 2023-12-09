module Main where

import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, eol, spaceChar)

parseSequences :: Parsec Void String [[Int]]
parseSequences = (some (digitChar <|> char '-') `sepBy1` char ' ' <&> map read) `sepBy1` eol

predict :: [Int] -> [Int] -> Int
predict next (x : y : rest) = predict ((y - x) : next) (y : rest)
predict next [x]
  | all (== 0) next = x
  | otherwise = x + predict [] (reverse next)

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  seqs <- readFile "input.txt" <&> doParse parseSequences
  print $ sum $ map (predict []) seqs
  print $ sum $ map (predict [] . reverse) seqs