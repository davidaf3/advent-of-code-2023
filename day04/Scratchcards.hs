module Main where

import Control.Monad (liftM)
import Data.Functor ((<&>))
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import qualified Data.Vector.Primitive.Mutable as MV
import Text.Parsec
  ( Parsec,
    char,
    digit,
    endBy,
    many1,
    parse,
    sepBy,
    space,
    string,
  )

data Card = Card Int (Set Int) (Set Int) deriving (Show)

parseCard :: Parsec String () Card
parseCard = do
  string "Card" >> many1 space
  cardId <- many1 digit <&> pred . read
  char ':' >> many1 space
  winning <- endBy (many1 digit) (many1 space) <&> map read
  string "|" >> many1 space
  mine <- sepBy (many1 digit) (many1 space) <&> map read
  return $ Card cardId (Set.fromList winning) (Set.fromList mine)

parseCards :: [String] -> [Card]
parseCards = map (either (error . show) id . parse parseCard "")

getMatches :: Card -> Int
getMatches (Card _ winning mine) = Set.size $ winning `intersection` mine

getPoints :: Card -> Int
getPoints card =
  let matches = getMatches card
   in if matches > 0 then 2 ^ (matches - 1) else 0

updateCopies :: (MV.PrimMonad m) => MV.MVector (MV.PrimState m) Int -> Card -> m ()
updateCopies copies card@(Card cId _ _) = do
  let won = filter (< MV.length copies) [cId + 1 .. cId + getMatches card]
  thisCardCopies <- MV.read copies cId
  mapM_ (MV.modify copies (+ thisCardCopies)) won

main :: IO ()
main = do
  cards <- readFile "input.txt" <&> parseCards . lines
  print $ sum $ map getPoints cards
  cardCopies <- MV.replicate (length cards) (1 :: Int)
  mapM_ (updateCopies cardCopies) cards
  MV.foldl (+) 0 cardCopies >>= print