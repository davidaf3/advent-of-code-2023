module Main where

import Data.Functor ((<&>))
import Data.List (insertBy, sortBy)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, eol)

type Point = (Int, Int, Int)

type Brick = (Point, Point)

parseBrick :: Parsec Void String Brick
parseBrick = do
  [xs, ys, zs] <- some digitChar `sepBy1` char ',' <&> map read
  char '~'
  [xe, ye, ze] <- some digitChar `sepBy1` char ',' <&> map read
  return ((xs, ys, zs), (xe, ye, ze))

overlapsXY :: Brick -> Brick -> Bool
overlapsXY ((xs1, ys1, _), (xe1, ye1, _)) ((xs2, ys2, _), (xe2, ye2, _)) =
  not ((xs1 > xe2 || xe1 < xs2) || (ys1 > ye2 || ye1 < ys2))

pushDown :: [Brick] -> Brick -> Brick
pushDown [] ((xs, ys, zs), (xe, ye, ze)) = ((xs, ys, 1), (xe, ye, 1 + (ze - zs)))
pushDown (top@(_, (_, _, ztop)) : rest) b@((xs, ys, zs), (xe, ye, ze)) =
  if overlapsXY top b
    then ((xs, ys, ztop + 1), (xe, ye, ztop + 1 + (ze - zs)))
    else pushDown rest b

compact ::
  (Brick -> Brick -> a -> a) -> [Brick] -> a -> (a, [Brick])
compact onPushDown bs acc =
  let sorted = sortBy (\((_, _, zs1), _) ((_, _, zs2), _) -> compare zs1 zs2) bs
   in go [] sorted acc
  where
    byZeReverse (_, (_, _, ze1)) (_, (_, _, ze2)) = compare ze2 ze1
    go compacted [] acc = (acc, compacted)
    go compacted (b : bs) acc =
      let pushedDown = pushDown compacted b
          compacted' = insertBy byZeReverse pushedDown compacted
          acc' = onPushDown b pushedDown acc
       in go compacted' bs acc'

fallenIfDisintegrated :: [Brick] -> Brick -> Int
fallenIfDisintegrated bs b =
  let withoutB = filter (/= b) bs
   in fst $ compact (\b b' acc -> if b /= b' then acc + 1 else acc) withoutB 0

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  bricks <- readFile "input.txt" <&> doParse (parseBrick `sepBy1` eol)
  let compacted = snd $ compact (\_ _ acc -> acc) bricks True
      fallenCount = map (fallenIfDisintegrated compacted) compacted
  print $ length $ filter (== 0) fallenCount
  print $ sum fallenCount