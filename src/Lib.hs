module Lib where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Control.Concurrent (threadDelay, forkIO)
import Control.Lens
import Control.Monad (void, forever)
import Control.Monad.State.Strict
import Control.Monad.State.Lazy as S
import Data.Char
import Data.Maybe
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import qualified Graphics.Vty as V

import BFTypes
import BFParsing
import BFInterpreter
import BFDisplay
import BFArchitectures
import BFController

runBrainfuckDebugger :: IO ()
runBrainfuckDebugger
    = do
        putStrLn "Where's your BF code?"
        filename <- getLine
        bfprog <- openFile filename ReadMode >>= hGetContents
        let bfprog' = filter (not . isSpace) bfprog
        let bfparsed = either (\_ -> BFProgram []) id (runParser bfParser 0 "" bfprog')
        let bfm = (bfInitMachine bf256ou 100) { bfstack = [bfparsed] }
        let bfvs = BFViewSettings {showCell = myShow, cellSpacing = 0}
        let bfdb = BFDebugger {
            bfmach = bfm, 
            bfstatus = BFOk, 
            debugstate = DebugStepping,
            bfinput = "ABCDabcd", 
            bfoutput = "",
            ticks = 0,
            debugconf = defaultDebugSettings
        }

        chan <- newBChan 10
        void $ forkIO $ forever $ do
            writeBChan chan TickerEvent
            threadDelay 1000

        void $ customMainWithDefaultVty (Just chan) (bfMakeApp bfvs) bfdb

