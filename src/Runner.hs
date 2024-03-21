module Runner where

import Lib
import Architectures

import Brick.BChan
import Brick.Widgets.Border
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Data.Char
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

{-

myShow :: Int -> String
myShow x = printf "%02x" x

someFunc :: IO ()
someFunc
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
            debugstate = DebugRunning,
            bfoutput = "",
            ticks = 0,
            debugconf = defaultDebugSettings
        }

        chan <- newBChan 10
        void $ forkIO $ forever $ do
            writeBChan chan TickerEvent
            threadDelay 1000

-}
