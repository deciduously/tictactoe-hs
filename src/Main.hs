module Main where

import           Control.Monad (forever, when)
import           Data.Bool     (bool)
import           Data.Char     (digitToInt)
import           Data.List     (isSubsequenceOf)
import           Data.Maybe    (isJust, isNothing)
import           System.Exit   (exitSuccess)
import           System.IO     (hFlush, stdout)
import           System.Random (randomRIO)

newtype Board = Board [Maybe Player]
data Player = Human | Computer deriving (Eq, Show)

instance Show Board where
  show (Board cs) = foldr spaceEachThird [].withIndicesFrom 1.fmap showCell $ withIndicesFrom 1 cs
    where spaceEachThird a = (++) (bool (snd a) (snd a ++ "\n") (fst a `rem` 3 == 0))

withIndicesFrom :: Int -> [a] -> [(Int, a)]
withIndicesFrom n = zip [n..]

showCell :: (Int, Maybe Player) -> String
showCell (n, Nothing)         = " " ++ show n ++ " "
showCell (_, (Just Human))    = " X "
showCell (_, (Just Computer)) = " O "

freshBoard :: Board
freshBoard = Board $ replicate 9 Nothing

isCellOpen :: Board -> Int -> Bool
isCellOpen (Board b) n = isNothing $ b !! (n - 1)

playCell :: Board -> Int -> Player -> Board
playCell (Board b) n m = Board $ take (n - 1) b ++ [Just m] ++ drop n b

compTurn :: Board -> IO Board
compTurn board@(Board b) = do
  let options = filter (isNothing.snd).withIndicesFrom 1 $ b
  r <- randomRIO (0, length options - 1)
  let play = (fst $ options !! r)
  let b2 = playCell board play Computer
  putStrLn $ "Computer plays " ++ show play
  checkWin b2 Computer
  return b2

handleInput :: Board -> Int -> IO Board
handleInput board n = do
  let b = playCell board n Human
  checkWin b Human
  checkDraw b
  return b

winStates :: [[Int]]
winStates = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]

checkWin :: Board -> Player -> IO ()
checkWin board@(Board b) m =
  let
    bi = withIndicesFrom 0 b
    plays = map fst.filter ((Just m==) . snd) $ bi
  in
   when (foldr ((||) . flip isSubsequenceOf plays) False winStates) $ do
     print board
     putStrLn $ show m ++ " won!"
     exitSuccess

checkDraw :: Board -> IO ()
checkDraw board@(Board b) =
  when ( all isJust b) $ do
    print board
    putStrLn "Draw!"
    exitSuccess

runGame :: Board -> IO ()
runGame board = forever $ do
  checkDraw board
  print board
  putStr "Your move: "
  hFlush stdout
  n <- getLine
  case n of
    [c] ->
      if [c] `elem` map show [(1::Integer)..9]
      then do
          let n' = digitToInt c
          if isCellOpen board n'
          then handleInput board n' >>= compTurn >>= runGame
          else putStrLn "That's taken!"
      else putStrLn "1-9 only please"
    _   -> putStrLn "Only one digit allowed!"

main :: IO ()
main = do
  let board = freshBoard
  runGame board
