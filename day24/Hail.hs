{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor (($>), (<&>))
import Data.List (intercalate)
import Data.Void (Void)
import Debug.Trace (trace)
import GaussElimination (solve)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, some)
import Text.Megaparsec.Char (char, eol, spaceChar, string)
import Text.Megaparsec.Char.Lexer (decimal, signed, space)

type Point = (Rational, Rational, Rational)

type Line = (Point, Point)

parseLine :: Parsec Void String Line
parseLine = do
  [x, y, z] <- number `sepBy1` (char ',' >> some spaceChar)
  some spaceChar >> char '@' >> some spaceChar
  [vx, vy, vz] <- number `sepBy1` (char ',' >> some spaceChar)
  return ((x, y, z), (vx, vy, vz))
  where
    number = signed (return ()) decimal

intersect :: Line -> Line -> Maybe (Rational, Rational)
intersect ((x1, y1, _), (vx1, vy1, _)) ((x2, y2, _), (vx2, vy2, _)) =
  let divisor = vy1 / vx1 - vy2 / vx2
      x = (y2 - (vy2 / vx2) * x2 - y1 + (vy1 / vx1) * x1) / divisor
      y = y1 + vy1 * ((x - x1) / vx1)
   in if divisor /= 0 then Just (x, y) else Nothing

intersectInside :: Rational -> Rational -> (Line, Line) -> Bool
intersectInside
  minCoord
  maxCoord
  (l1@((x1, y1, _), (vx1, vy1, _)), l2@((x2, y2, _), (vx2, vy2, _))) =
    case l1 `intersect` l2 of
      Just (x, y) ->
        x >= minCoord
          && x <= maxCoord
          && y >= minCoord
          && y <= maxCoord
          && compare x x1 == compare vx1 0
          && compare y y1 == compare vy1 0
          && compare x x2 == compare vx2 0
          && compare y y2 == compare vy2 0
      _ -> False

pairs :: [a] -> [(a, a)]
pairs [_] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

extractXY :: Line -> (Rational, Rational, Rational, Rational)
extractXY ((x, y, _), (vx, vy, _)) = (x, y, vx, vy)

extractXZ :: Line -> (Rational, Rational, Rational, Rational)
extractXZ ((x, _, z), (vx, _, vz)) = (x, z, vx, vz)

toEqSystem :: [Line] -> (Line -> (Rational, Rational, Rational, Rational)) -> [[Rational]]
toEqSystem lines extract = go $ take 4 $ pairs $ map extract lines
  where
    go [] = []
    go
      (((a1, b1, va1, vb1), (a2, b2, va2, vb2)) : ls) =
        let eq =
              [ vb2 - vb1,
                va1 - va2,
                b1 - b2,
                a2 - a1,
                a2 * vb2 - b2 * va2 - a1 * vb1 + b1 * va1
              ]
         in eq : go ls

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  lines <- readFile "input.txt" <&> doParse (parseLine `sepBy1` eol)
  print $ length $ filter (intersectInside 200000000000000 400000000000000) $ pairs lines
  let [x, y, _, _] = solve $ toEqSystem lines extractXY
      [_, z, _, _] = solve $ toEqSystem lines extractXZ
  print $ round $ x + y + z