{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.SBV (Symbolic, constrain, getModelValue, sInteger, sat, (.==), (.>=))
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, some)
import Text.Megaparsec.Char (char, eol, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Point a = (a, a, a)

type Line a = (Point a, Point a)

parseLine :: Parsec Void String (Line Int)
parseLine = do
  [x, y, z] <- number `sepBy1` (char ',' >> some spaceChar)
  some spaceChar >> char '@' >> some spaceChar
  [vx, vy, vz] <- number `sepBy1` (char ',' >> some spaceChar)
  return ((x, y, z), (vx, vy, vz))
  where
    number = signed (return ()) decimal

toRat :: Line Int -> Line Rational
toRat ((x, y, z), (vx, vy, vz)) =
  ( (fromIntegral x, fromIntegral y, fromIntegral z),
    (fromIntegral vx, fromIntegral vy, fromIntegral vz)
  )

intersect :: Line Rational -> Line Rational -> Maybe (Rational, Rational)
intersect ((x1, y1, _), (vx1, vy1, _)) ((x2, y2, _), (vx2, vy2, _)) =
  let divisor = vy1 / vx1 - vy2 / vx2
      x = (y2 - (vy2 / vx2) * x2 - y1 + (vy1 / vx1) * x1) / divisor
      y = y1 + vy1 * ((x - x1) / vx1)
   in if divisor /= 0 then Just (x, y) else Nothing

intersectInside :: Rational -> Rational -> (Line Rational, Line Rational) -> Bool
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

findIntersecting :: [Line Int] -> Symbolic ()
findIntersecting lines = do
  [x, y, z, vx, vy, vz] <- mapM sInteger ["x", "y", "z", "vx", "vy", "vz"]
  let constrainLine (i, ((xi, yi, zi), (vxi, vyi, vzi))) = do
        ti <- sInteger ('t' : show i)
        constrain $ ti .>= 0
        constrain $ x + vx * ti .== fromIntegral xi + fromIntegral vxi * ti
        constrain $ y + vy * ti .== fromIntegral yi + fromIntegral vyi * ti
        constrain $ z + vz * ti .== fromIntegral zi + fromIntegral vzi * ti
  foldMapM constrainLine $ zip [1 ..] $ take 3 lines

foldMapM :: (Monad m) => (a -> m b) -> [a] -> m b
foldMapM _ [] = error "foldMapM: empty list"
foldMapM f [x] = f x
foldMapM f (x : xs) = f x >> foldMapM f xs

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  lines <- readFile "input.txt" <&> doParse (parseLine `sepBy1` eol)
  let ratPairs = pairs $ map toRat lines
  print $ length $ filter (intersectInside 200000000000000 400000000000000) ratPairs
  model <- sat (findIntersecting lines)
  print $ sum (map (fromJust . flip getModelValue model) ["x", "y", "z"] :: [Integer])