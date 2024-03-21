module BFDisplay where

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
import Text.Printf

import qualified Graphics.Vty as V

import BFTypes

-- -- -- -- -- --
-- DISPLAYING  --
-- -- -- -- -- --

data BFViewSettings cell = BFViewSettings {
    showCell    :: cell -> String,
    cellSpacing :: Int
}

bfShowMem :: BFViewSettings cell -> BFMachine cell buf -> String
bfShowMem conf bfm
    = let i = headpos bfm in
      foldr (\(j,x) y -> showCell conf x ++ (if i == j then "< " else "  ") ++ y) "" (zip [0..] $ cells bfm)

bfShowMemPane :: BFViewSettings cell -> BFDebugger cell buf -> Widget ()
bfShowMemPane bfvs = strWrap . (bfShowMem bfvs) . bfmach

bfShowMemInfoPane :: BFViewSettings cell -> BFDebugger cell buf -> Widget ()
bfShowMemInfoPane bfvs bfdb
    = strWrap $
        "Head position: " ++ show (headpos $ bfmach bfdb)

bfShowDbgPane :: BFViewSettings cell -> BFDebugger cell buf -> Widget ()
bfShowDbgPane bfvs bfdb = str $
    "Milliseconds per step: " ++ show (msPerStep $ debugconf bfdb) ++ "\n" ++
    "Debug state: " ++ show (debugstate bfdb) ++ "\n" ++
    case (bfstatus bfdb) of
        BFOk -> "Running..."
        BFError msg -> msg
        BFPause n -> "Stopped at breakpoint " ++ show n

bfShowOutPane :: BFViewSettings cell -> BFDebugger cell buf -> Widget ()
bfShowOutPane bfvs bfdb = str $ "Output: " ++ filter isPrint (bfoutput bfdb)

bfShowProg :: BFProgram -> String
bfShowProg (BFProgram prog) = go "" prog
    where
        go acc [] = acc
        go acc (cmd:cmds) =
            case cmd of
                BFLeft -> go ('>':acc) cmds
                BFRight -> go ('<':acc) cmds
                BFPlus -> go ('+':acc) cmds
                BFMinus -> go ('-':acc) cmds
                BFGet -> go (',':acc) cmds
                BFPut -> go ('.':acc) cmds
                BFLoop prog -> go ('[':acc ++ "]") cmds

bfTitle :: BFDebugger cell buf -> Widget ()
bfTitle bfdb = strWrap $
    archName (bfarch $ bfmach bfdb) ++ " VIRTUAL MACHINE\n" ++
    archNameVerbose (bfarch $ bfmach bfdb)

bfInstructions :: Widget ()
bfInstructions = strWrap $
    "Press R to continue running the simulator.\n" ++
    "Use the up and down arrow keys to adjust debugger speed.\n" ++
    "Press S to step through the simulation.\n" ++
    "Press J to jump forward to your next break point." 

bfUI :: BFViewSettings cell -> BFDebugger cell buf -> [Widget ()]
bfUI bfvs bfdb 
    = [
        (border $ 
            bfTitle bfdb
            <=> hBorder
            <=> bfShowMemPane bfvs bfdb
            <=> hBorder 
            <=> (bfShowOutPane bfvs bfdb))
        <=> bfShowMemInfoPane bfvs bfdb
        <=> bfShowDbgPane bfvs bfdb 
        <=> border bfInstructions
    ]

myShow :: Int -> String
myShow x = printf "%02x" x

