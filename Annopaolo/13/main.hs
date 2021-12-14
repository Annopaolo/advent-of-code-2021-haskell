{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Char
import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1", input] -> part1 input
    ["part2", input] -> part2 input

data Direction a = Horizontal a | Vertical a

type Point = (Int, Int)

parse :: (Read b, Read a) => [Char] -> ([(b, b)], [Direction a])
parse xs =
  let [spoints, sfolds] = splitWhen (== "") . lines $ xs
      points = toTuple . map read . (splitOn ",") <$> spoints
      folds = toDirection . readFold . concatMap (filter isCoordinate) . words <$> sfolds
   in (points, folds)
  where
    toTuple [x, y] = (x, y)
    readFold (x : ns) = (x, read ns)
    isCoordinate x = or $ [isDigit, (== 'x'), (== 'y')] <*> pure x
    toDirection ('x', n) = Horizontal n
    toDirection ('y', n) = Vertical n

symmetric :: Num a => Direction a -> (a, a) -> (a, a)
symmetric (Horizontal z) (x, y) = (2 * z - x, y)
symmetric (Vertical z) (x, y) = (x, 2 * z - y)

foldOnDirection :: (Ord a, Num a) => [(a, a)] -> Direction a -> [(a, a)]
foldOnDirection points axis = map (doFlip axis) points
  where
    doFlip a@(Vertical n) p@(_, y) = if y > n then symmetric a p else p
    doFlip a@(Horizontal n) p@(x, _) = if x > n then symmetric a p else p

solve :: ([Point], [Direction Int]) -> [Point]
solve (points, folds) = foldl' foldOnDirection points [head folds]

part1 :: FilePath -> IO ()
part1 input = putStrLn . show . length . nub . solve . parse =<< readFile input

solve' :: [Point] -> [Direction Int] -> [Point]
solve' = foldl' foldOnDirection

display :: [Point] -> String
display points = unlines . map (map toDot) $ [[(x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    maxX = maximum [x | (x, _) <- points]
    maxY = maximum [y | (_, y) <- points]
    toDot p = if p `elem` points then '▒' else ' '

part2 :: FilePath -> IO ()
part2 input = putStrLn . display . uncurry solve' . parse =<< readFile input
