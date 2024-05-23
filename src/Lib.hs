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
import System.Environment
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
        args <- getArgs
        let codefile = if null args then "" else args !! 0
        codehandle <- openFile codefile ReadMode
        (optArch, optSize, inFile) <- fmap bfParseRunOpts $ hGetLine codehandle
        let nCells = read optSize
        inHandle <- openFile inFile ReadMode
        bfprog <- hGetContents codehandle
        stringIn <- hGetContents inHandle
        let bfprog' = filter (not . isSpace) bfprog
        let bfparsed = either (\_ -> BFProgram []) id (runParser bfParser 0 "" bfprog')
        let bfvs = BFViewSettings {showCell = undefined, cellSpacing = 0}
        let bfdb = BFDebugger {
            bfmach = undefined, 
            bfstatus = BFOk, 
            debugstate = DebugStepping,
            bfinput = stringIn, 
            bfoutput = "",
            ticks = 0,
            debugconf = defaultDebugSettings
        }

        chan <- newBChan 10
        void $ forkIO $ forever $ do
            writeBChan chan TickerEvent
            threadDelay 1000

        case optArch of
            "bf256ou"   
                -> void $ customMainWithDefaultVty (Just chan) (bfMakeApp $ bfvs { showCell = myShow }) (bfdb { bfmach = (bfInitMachine bf256ou nCells) { bfstack = [bfparsed] } })
            "bfNat"     
                -> void $ customMainWithDefaultVty (Just chan) (bfMakeApp $ bfvs { showCell = show }) (bfdb { bfmach = (bfInitMachine bfNat nCells) { bfstack = [bfparsed] } })
            "bf2ou"     
                -> void $ customMainWithDefaultVty (Just chan) (bfMakeApp $ bfvs { showCell = \b -> if b then "1" else "0" }) (bfdb { bfmach = (bfInitMachine bf2ou nCells) { bfstack = [bfparsed] } })
            _           -> putStrLn $ "Invalid architecture: " ++ optArch
        return ()

runBrainfuckTranspiler :: (String -> String) -> IO ()
runBrainfuckTranspiler tr
    = do
        args <- getArgs
        let codefile = if null args then "" else args !! 0
        codehandle <- openFile codefile ReadMode
        contents <- hGetContents codehandle
        putStr (tr contents) 
