module Main where

import Data.Functor ((<&>))
import Data.List (sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, errorBundlePretty, oneOf, parse, sepBy1, some)
import Text.Megaparsec.Char (digitChar, eol, spaceChar)

data Card = Card Char | Joker deriving (Show)

data HandType = Five | Four | Full | Three | Pair2 | Pair | High deriving (Show)

data Hand = Hand [Card] HandType Int deriving (Show)

handTypeOrder :: HandType -> Int
handTypeOrder Five = 6
handTypeOrder Four = 5
handTypeOrder Full = 4
handTypeOrder Three = 3
handTypeOrder Pair2 = 2
handTypeOrder Pair = 1
handTypeOrder High = 0

cardOrder :: Card -> Int
cardOrder (Card 'A') = 12
cardOrder (Card 'K') = 11
cardOrder (Card 'Q') = 10
cardOrder (Card 'J') = 9
cardOrder (Card 'T') = 8
cardOrder (Card '9') = 7
cardOrder (Card '8') = 6
cardOrder (Card '7') = 5
cardOrder (Card '6') = 4
cardOrder (Card '5') = 3
cardOrder (Card '4') = 2
cardOrder (Card '3') = 1
cardOrder (Card '2') = 0
cardOrder Joker = -1
cardOrder (Card x) = error $ "Invalid card: " ++ [x]

instance Eq Hand where
  (==) (Hand cs1 ht1 _) (Hand cs2 ht2 _) = (ht1 == ht2) && (cs1 == cs2)

instance Ord Hand where
  compare (Hand cs1 ht1 _) (Hand cs2 ht2 _) = case compare ht1 ht2 of
    EQ -> compare cs1 cs2
    x -> x

instance Eq HandType where
  (==) ht1 ht2 = handTypeOrder ht1 == handTypeOrder ht2

instance Ord HandType where
  compare ht1 ht2 = compare (handTypeOrder ht1) (handTypeOrder ht2)

instance Eq Card where
  (==) c1 c2 = cardOrder c1 == cardOrder c2

instance Ord Card where
  compare c1 c2 = compare (cardOrder c1) (cardOrder c2)

parseHand :: Parsec Void String Hand
parseHand = do
  cards <- some (oneOf "AKQJT98765432") <&> map Card
  spaceChar
  bid <- some digitChar <&> read
  return $ Hand cards (getHandType cards False) bid

parseHands :: Parsec Void String [Hand]
parseHands = parseHand `sepBy1` eol

getHandType :: [Card] -> Bool -> HandType
getHandType cards withJoker =
  let cardsMap = foldr countCard Map.empty cards
      cardinality = Map.size cardsMap
      jokerCount = fromMaybe 0 $ Map.lookup 'J' cardsMap
      withJoker' = withJoker && jokerCount > 0
      cardinality' = if not withJoker' then cardinality else cardinality - 1
      count = Map.toList cardsMap
      count' =
        if not withJoker'
          then count
          else case sortBy (\(_, c1) (_, c2) -> compare c2 c1) $ filter ((/= 'J') . fst) count of
            [] -> []
            ((ch, c) : rst) -> (ch, c + jokerCount) : rst
   in case cardinality' of
        0 -> Five -- Five jokers
        1 -> Five
        2 -> if any ((== 4) . snd) count' then Four else Full
        3 -> if any ((== 3) . snd) count' then Three else Pair2
        4 -> Pair
        5 -> High
        _ -> error "Invalid hand"
  where
    countCard (Card c) = Map.alter (Just . (+ 1) . fromMaybe 0) c

getWinnings :: [Hand] -> Int
getWinnings = sum . zipWith (curry (\(r, Hand _ _ b) -> r * b)) [1 ..] . sort

withJokers :: Hand -> Hand
withJokers (Hand cs _ b) = Hand (map toJoker cs) (getHandType cs True) b
  where
    toJoker c@(Card ch) = if ch == 'J' then Joker else c

doParse :: Parsec Void String a -> String -> a
doParse parser = either (error . errorBundlePretty) id . parse parser ""

main :: IO ()
main = do
  hands <- readFile "input.txt" <&> doParse parseHands
  print $ getWinnings hands
  print $ getWinnings $ map withJokers hands