{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor (($>), (<&>))
import Data.List (foldl', sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, optional, parse, sepBy1, some, (<|>))
import Text.Megaparsec.Char (char, eol, letterChar, string)

data Pulse = Pulse
  { pulseSrc :: String,
    pulseBit :: Bool,
    pulseDst :: String
  }
  deriving (Show)

data ModuleKind
  = FlipFlop Bool
  | Conjunction (Map String Bool)
  | Broadcast
  deriving (Show)

data Module = Module
  { modName :: String,
    modKind :: ModuleKind,
    modOut :: [String]
  }
  deriving (Show)

parseModule :: Parsec Void String (String, Module)
parseModule = do
  kind <-
    optional ((char '%' $> FlipFlop False) <|> (char '&' $> Conjunction Map.empty))
      <&> fromMaybe Broadcast
  name <- some letterChar
  out <- string " -> " >> some letterChar `sepBy1` string ", "
  return (name, Module name kind out)

processPulse :: Pulse -> Map String Module -> ([Pulse], Map String Module)
processPulse (Pulse src bit dst) ms =
  case Map.lookup dst ms of
    Nothing -> ([], ms)
    Just m@(Module name kind out) -> case kind of
      Broadcast -> (map (Pulse name bit) out, ms)
      (FlipFlop s) ->
        if bit
          then ([], ms)
          else
            ( map (Pulse name (not s)) out,
              Map.insert name (m {modKind = FlipFlop (not s)}) ms
            )
      (Conjunction mem) ->
        let mem' = Map.insert src bit mem
         in ( map (Pulse name (not $ and mem')) out,
              Map.insert name (m {modKind = Conjunction mem'}) ms
            )

pressButton ::
  (Pulse -> a -> a) ->
  Map String Module ->
  a ->
  (Map String Module, a)
pressButton forEachPulse = go [Pulse "button" False "broadcaster"]
  where
    go [] ms acc = (ms, acc)
    go (p : ps) ms acc =
      let acc' = forEachPulse p acc
          (ps', ms') = processPulse p ms
       in go (ps ++ ps') ms' acc'

pressTill ::
  (Int -> a -> Bool) ->
  (Int -> Pulse -> a -> a) ->
  Map String Module ->
  a ->
  a
pressTill stop forEachPulse ms acc = go ms 0 acc
  where
    go ms i acc
      | stop i acc = acc
      | otherwise =
          let (ms', acc') = pressButton (forEachPulse i) ms acc
           in go ms' (i + 1) acc'

initConjunction :: [(String, Module)] -> (String, Module) -> (String, Module)
initConjunction ms (name, m@(Module _ (Conjunction mem) _)) =
  let inputs = map (,False) $ getInputs ms name
   in (name, m {modKind = Conjunction (Map.fromList inputs)})
initConjunction _ x = x

getInputs :: [(String, Module)] -> String -> [String]
getInputs ms name = map fst $ filter (elem name . modOut . snd) ms

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  mList <- readFile "input.txt" <&> doParse (parseModule `sepBy1` eol)
  let ms = Map.fromList $ map (initConjunction mList) mList
      countPulse _ p (l, h) = if pulseBit p then (l, h + 1) else (l + 1, h)
      (ls, hs) = pressTill (\i _ -> i == 1000) countPulse ms (0, 0)
      rxInputs = concatMap (getInputs mList) $ getInputs mList "rx"
      rxInputsLen = length rxInputs
      addCycle i (Pulse src bit _) cycles
        | not bit || notElem src rxInputs = cycles
        | otherwise = Map.alter (Just . fromMaybe (i + 1)) src cycles
      cycles = pressTill (\_ m -> Map.size m == rxInputsLen) addCycle ms Map.empty
  print $ ls * hs
  print $ foldl' lcm 1 $ Map.elems cycles