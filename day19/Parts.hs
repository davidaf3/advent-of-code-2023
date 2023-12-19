module Main where

import Data.Either (partitionEithers)
import Data.Foldable (find)
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    endBy1,
    errorBundlePretty,
    oneOf,
    parse,
    sepBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)

data Part a = Part
  { partX :: a,
    partM :: a,
    partA :: a,
    partS :: a
  }
  deriving (Show)

type PartRange = Part (Int, Int)

data Result = Continue String | Reject | Accept deriving (Show)

data Rule = Rule
  { ruleCat :: Char,
    ruleComp :: Char,
    ruleVal :: Int,
    ruleRes :: Result
  }
  deriving (Show)

data Workflow = Workflow [Rule] Result

isAccept :: Result -> Bool
isAccept Accept = True
isAccept _ = False

getCombinations :: PartRange -> Int
getCombinations (Part (xs, xe) (ms, me) (as, ae) (ss, se)) =
  (xe - xs + 1) * (me - ms + 1) * (ae - as + 1) * (se - ss + 1)

passesRule :: Part Int -> Rule -> Bool
passesRule (Part x m a s) (Rule cat comp val _) =
  let rating = case cat of
        'x' -> x
        'm' -> m
        'a' -> a
        's' -> s
      compareFn = case comp of
        '<' -> (<)
        '>' -> (>)
   in rating `compareFn` val

partitionRange :: PartRange -> Rule -> (Maybe PartRange, Maybe PartRange)
partitionRange part@(Part x m a s) (Rule cat comp val _) =
  let (start, end) = case cat of
        'x' -> x
        'm' -> m
        'a' -> a
        's' -> s
      (acc, rej) = case comp of
        '>' ->
          if start > val
            then (Just (start, end), Nothing)
            else
              ( if (end < val) || (start == end)
                  then (Nothing, Just (start, end))
                  else (Just (val + 1, end), Just (start, val))
              )
        '<' ->
          if end < val
            then (Just (start, end), Nothing)
            else
              ( if (start > val) || (start == end)
                  then (Nothing, Just (start, end))
                  else (Just (start, val - 1), Just (val, end))
              )
   in case cat of
        'x' -> (acc <&> (\r -> part {partX = r}), rej <&> (\r -> part {partX = r}))
        'm' -> (acc <&> (\r -> part {partM = r}), rej <&> (\r -> part {partM = r}))
        'a' -> (acc <&> (\r -> part {partA = r}), rej <&> (\r -> part {partA = r}))
        's' -> (acc <&> (\r -> part {partS = r}), rej <&> (\r -> part {partS = r}))

parseWorkflow :: Parsec Void String (String, Workflow)
parseWorkflow = do
  name <- some letterChar
  char '{'
  elems <- ((try parseRule <&> Left) <|> (parseResult <&> Right)) `sepBy1` char ','
  char '}'
  let (rules, results) = partitionEithers elems
  return (name, Workflow rules (head results))
  where
    parseRule = do
      cat <- oneOf "xmas"
      comp <- oneOf "<>"
      val <- some digitChar <&> read
      res <- char ':' >> parseResult
      return $ Rule cat comp val res
    parseResult =
      (char 'A' $> Accept)
        <|> (char 'R' $> Reject)
        <|> (some letterChar <&> Continue)

parsePart :: Parsec Void String (Part Int)
parsePart = do
  char '{'
  x <- string "x=" >> some digitChar <&> read
  m <- char ',' >> string "m=" >> some digitChar <&> read
  a <- char ',' >> string "a=" >> some digitChar <&> read
  s <- char ',' >> string "s=" >> some digitChar <&> read
  char '}'
  return $ Part x m a s

parseInput :: Parsec Void String (Map String Workflow, [Part Int])
parseInput = do
  workflows <- parseWorkflow `endBy1` eol
  parts <- eol >> parsePart `sepBy1` eol
  return (Map.fromList workflows, parts)

applyWorkflow :: Map String Workflow -> String -> Part Int -> Result
applyWorkflow workflows name part =
  let (Workflow rules ifNoMatch) =
        fromMaybe (error "Workflow not found") $ Map.lookup name workflows
      res = maybe ifNoMatch ruleRes $ find (passesRule part) rules
   in case res of
        (Continue nextName) -> applyWorkflow workflows nextName part
        x -> x

getAccepted :: Map String Workflow -> String -> PartRange -> Int
getAccepted workflows name part =
  let (Workflow rules ifNoMatch) =
        fromMaybe (error "Workflow not found") $ Map.lookup name workflows
   in applyRulesRange rules ifNoMatch part
  where
    applyRulesRange :: [Rule] -> Result -> PartRange -> Int
    applyRulesRange [] res part = applyResult res part
    applyRulesRange (r@(Rule _ _ _ ruleRes) : rs) res part =
      let (accepted, rejected) = partitionRange part r
       in maybe 0 (applyResult ruleRes) accepted
            + maybe 0 (applyRulesRange rs res) rejected
    applyResult Reject _ = 0
    applyResult Accept part = getCombinations part
    applyResult (Continue nextName) part = getAccepted workflows nextName part

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  (workflows, parts) <- readFile "input.txt" <&> doParse parseInput
  let accepted = filter (isAccept . applyWorkflow workflows "in") parts
  print $ sum $ map (\(Part x m a s) -> x + m + a + s) accepted
  print $ getAccepted workflows "in" (Part (1, 4000) (1, 4000) (1, 4000) (1, 4000))