module Main where

import Data.Char (ord)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO as TIO (readFile)
import qualified Data.Text.Read as TR
import Debug.Trace (trace)

data Instruction = Instruction T.Text InstructionType deriving (Show)

data InstructionType = Set Int | Delete deriving (Show)

type Box = [(T.Text, Int)]

type Boxes = Map Int Box

parseInstrucion :: T.Text -> Instruction
parseInstrucion inst =
  let (label, value) = T.break (== '=') inst
   in if T.null value
        then Instruction (T.take (T.length label - 1) label) Delete
        else
          Instruction label $
            Set (either (error "Parse error") fst $ TR.decimal $ T.tail value)

hash :: T.Text -> Int
hash = T.foldl' (\h c -> ((h + ord c) * 17) `mod` 256) 0

run :: Boxes -> Instruction -> Boxes
run boxes (Instruction label instType) =
  let f = case instType of
        (Set value) -> set label value
        Delete -> delete label
   in Map.alter (Just . f . fromMaybe []) (hash label) boxes

delete :: T.Text -> Box -> Box
delete label [] = []
delete label (x@(other, _) : xs)
  | label == other = xs
  | otherwise = x : delete label xs

set :: T.Text -> Int -> Box -> Box
set label value [] = [(label, value)]
set label value (x@(other, _) : xs)
  | label == other = (label, value) : xs
  | otherwise = x : set label value xs

getFocusingPower :: (Int, Box) -> Int
getFocusingPower (i, b) = (i + 1) * sum (zipWith (*) [1 ..] $ map snd b)

main :: IO ()
main = do
  initSeq <- TIO.readFile "input.txt" <&> T.split (== ',')
  print $ sum $ map hash initSeq
  let boxes = foldl run Map.empty $ map parseInstrucion initSeq
  print $ sum $ map getFocusingPower $ Map.toList boxes