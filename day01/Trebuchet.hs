module Main where

import Data.Char (isDigit)
import Data.Functor ((<&>))

getCalibration :: (String -> [Char]) -> String -> Int
getCalibration extractDigits line = case extractDigits line of
  [] -> error "Parse error"
  digits -> read [head digits, last digits]

matchWord :: String -> String -> Bool
matchWord _ "" = True
matchWord "" _ = False
matchWord (t : ts) (w : ws)
  | t == w = matchWord ts ws
  | otherwise = False

digitWords :: [(String, Char)]
digitWords =
  [ ("one",   '1'),
    ("two",   '2'),
    ("three", '3'),
    ("four",  '4'),
    ("five",  '5'),
    ("six",   '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine",  '9')
  ]

matchDigitWords :: String -> [(String, Char)] -> Maybe Char
matchDigitWords text [] = Nothing
matchDigitWords text ((word, digit) : xs)
  | matchWord text word = Just digit
  | otherwise = matchDigitWords text xs

filterDigitsOrWords :: String -> [Char]
filterDigitsOrWords "" = ""
filterDigitsOrWords text@(x : xs)
  | isDigit x = x : filterDigitsOrWords xs
  | otherwise = case matchDigitWords text digitWords of
      (Just digit) -> digit : filterDigitsOrWords xs
      Nothing -> filterDigitsOrWords xs

main :: IO ()
main = do
  inputLines <- readFile "input.txt" <&> lines
  print $ sum $ map (getCalibration (filter isDigit)) inputLines
  print $ sum $ map (getCalibration filterDigitsOrWords) inputLines