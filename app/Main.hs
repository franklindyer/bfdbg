module Main (main) where

import BFParsing
import Lib

main :: IO ()
main = runBrainfuckDebugger
-- main = runBrainfuckTranspiler bf256ouTo2ouTranspiler
