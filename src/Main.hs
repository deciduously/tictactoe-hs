module Main where

import TicTacToeLib

main :: IO ()
main = do
  let board = freshBoard
  runGame board
