module Main where

import Data.Functor (($>), (<&>))
import Text.Parsec (Parsec, char, digit, many1, parse, sepBy, space, string, (<|>))

data Color = Red | Green | Blue deriving (Show)

data Cubes = Cubes Int Color deriving (Show)

data Game = Game
  { gameId :: Int,
    gameCubeSets :: [[Cubes]]
  }
  deriving (Show)

parseCubes :: Parsec String () Cubes
parseCubes = do
  number <- many1 digit
  space
  color <-
    (string "red" $> Red)
      <|> (string "green" $> Green)
      <|> (string "blue" $> Blue)
  return $ Cubes (read number) color

parseCubesSet :: Parsec String () [Cubes]
parseCubesSet = sepBy parseCubes (string ", ")

parseGame :: Parsec String () Game
parseGame = do
  string "Game "
  gameId <- many1 digit
  string ": "
  cubeSets <- sepBy parseCubesSet (string "; ")
  return $ Game (read gameId) cubeSets

parseGames :: [String] -> [Game]
parseGames = map (either (error . show) id . parse parseGame "")

isValid :: Game -> Bool
isValid (Game _ cubeSets) = all (all isValidCubes) cubeSets
  where
    isValidCubes (Cubes n Red) = n <= 12
    isValidCubes (Cubes n Green) = n <= 13
    isValidCubes (Cubes n Blue) = n <= 14

fewestCubes :: Game -> (Int, Int, Int)
fewestCubes (Game _ cubeSets) = foldl (foldl updateFewest) (0, 0, 0) cubeSets
  where
    updateFewest (r, g, b) (Cubes n Red) = (max n r, g, b)
    updateFewest (r, g, b) (Cubes n Green) = (r, max n g, b)
    updateFewest (r, g, b) (Cubes n Blue) = (r, g, max n b)

power :: (Int, Int, Int) -> Int
power (r, g, b) = r * g * b

main :: IO ()
main = do
  games <- readFile "input.txt" <&> parseGames . lines
  print $ sum $ map gameId $ filter isValid games
  print $ sum $ map (power . fewestCubes) games