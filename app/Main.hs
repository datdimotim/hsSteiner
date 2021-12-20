module Main where

import Lib
import Steiner

main :: IO ()
main = print . snd $ bench
