module Main where

import Data.Functor (($>), (<&>))
import Data.List (transpose)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    endBy1,
    eof,
    errorBundlePretty,
    oneOf,
    parse,
    sepBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (eol)

diff :: String -> String -> Int
diff [] [] = 0
diff (x : xs) (y : ys) = (if x == y then 0 else 1) + diff xs ys
diff _ _ = error "Strings must have the same length"

parseGrids :: Parsec Void String [(Vector String, Vector String)]
parseGrids = parseGrid `sepBy1` eol
  where
    parseGrid = do
      grid <- some (oneOf ".#") `endBy1` ((eol $> ()) <|> eof)
      return (V.fromList grid, V.fromList $ transpose grid)

getReflectionLines :: Bool -> Vector String -> [Int]
getReflectionLines useSmudge v = filter (reflects v useSmudge) [1 .. V.length v - 1]

reflects :: Vector String -> Bool -> Int -> Bool
reflects v useSmudge i = go (i - 1) i False
  where
    go li ri smudgeUsed = case (v !? li, v !? ri) of
      (Just l, Just r) -> case diff l r of
        0 -> go (li - 1) (ri + 1) smudgeUsed
        1 -> (useSmudge && not smudgeUsed) && go (li - 1) (ri + 1) True
        _ -> False
      _ -> not useSmudge || smudgeUsed

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  grids <- readFile "input.txt" <&> doParse parseGrids
  let cols = concatMap (getReflectionLines False . snd) grids
      rows = concatMap (getReflectionLines False . fst) grids
  print $ sum cols + 100 * sum rows
  let cols' = concatMap (getReflectionLines True . snd) grids
      rows' = concatMap (getReflectionLines True . fst) grids
  print $ sum cols' + 100 * sum rows'