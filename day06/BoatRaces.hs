module Main where

import Data.Functor ((<&>))
import Text.Parsec
  ( Parsec,
    digit,
    endBy1,
    endOfLine,
    many1,
    parse,
    sepBy1,
    spaces,
    string,
  )

data Race = Race Int Int deriving (Show)

parseRaces :: Parsec String () ([Race], Race)
parseRaces = do
  ts <- string "Time:" >> spaces >> many1 digit `endBy1` spaces
  ds <- string "Distance:" >> spaces >> many1 digit `sepBy1` spaces
  let withSpaces = [Race (read t) (read d) | (t, d) <- zip ts ds]
  let withoutSpaces = Race (read $ concat ts) (read $ concat ds)
  return (withSpaces, withoutSpaces)

getWinningWays :: Race -> Int
getWinningWays r@(Race t record) =
  let (maxW, minW) = (go (-) t, go (+) 0)
   in if maxW > t || minW < 0 then 0 else maxW - minW + 1
  where
    go f bt
      | bt > t || bt < 0 = bt
      | (t - bt) * bt > record = bt
      | otherwise = go f (bt `f` 1)

main :: IO ()
main = do
  (rs, r) <- readFile "input.txt" <&> either (error . show) id . parse parseRaces ""
  print $ product $ map getWinningWays rs
  print $ getWinningWays r