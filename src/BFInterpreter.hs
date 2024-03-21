module BFInterpreter where

import Control.Lens
import Control.Monad (void, forever)
import Control.Monad.State.Strict
import Control.Monad.State.Lazy as S
import Data.Char
import Data.Maybe
import System.IO

import BFTypes

-- -- -- -- -- -- --
-- INTERPRETING   --
-- -- -- -- -- -- --

cLEFT_OOB_ERRORMSG  = "Head moved out of bounds to the left"
cRIGHT_OOB_ERRORMSG = "Head moved out of bounds to the right"
cNO_INC_ERRORMSG    = "Could not increment cell"
cNO_DEC_ERRORMSG    = "Could not decrement cell"
cEND_OF_CODE_MSG    = "Code is finished running."

bfInitMachine :: BFArchitecture cell buf -> Int -> BFMachine cell buf
bfInitMachine arch size 
    = BFMachine {
        bfarch = arch,
        time = 0,
        ncells = size,
        headpos = 0,
        cells = replicate size (bfZero arch),
        bufin = bfEmptyBuf arch,
        bufout = bfEmptyBuf arch,
        bfstack = []
    }

bfRequire :: String -> Bool -> BFResult
bfRequire msg cond = if cond then BFOk else BFError msg 

runCommand :: Eq cell => BFCommand -> BFMachMonad cell buf BFResult
runCommand cmd
    = do
        case cmd of
            BFLeft -> state (\bfm -> 
                (bfRequire cLEFT_OOB_ERRORMSG $ headpos bfm > 0 , 
                 bfm { headpos = headpos bfm - 1 }))
            BFRight -> state (\bfm ->
                (bfRequire cRIGHT_OOB_ERRORMSG $ headpos bfm < ncells bfm - 1, 
                 bfm { headpos = headpos bfm + 1 }))
            BFPlus -> state (\bfm ->
                maybe
                    (BFError cNO_INC_ERRORMSG , bfm)
                    (\newc -> (BFOk , bfm { cells = cells bfm & element (headpos bfm) .~ newc }))
                    (incrementCell (bfarch bfm) (cells bfm !! headpos bfm))) 
            BFMinus -> state (\bfm ->
                maybe
                    (BFError cNO_DEC_ERRORMSG , bfm)
                    (\newc -> (BFOk , bfm { cells = cells bfm & element (headpos bfm) .~ newc }))
                    (decrementCell (bfarch bfm) (cells bfm !! headpos bfm)))
            BFGet -> state (\bfm ->
                let (buf' , newc) = readToCell (bfarch bfm) $ bufin bfm in
                (BFOk , bfm { bufin = buf', cells = cells bfm & element (headpos bfm) .~ newc }))
            BFPut -> state (\bfm ->
                (BFOk , bfm { bufout = writeFromCell (bfarch bfm) (bufout bfm) (cells bfm !! headpos bfm) }))
            BFLoop (BFProgram bfprog) -> state (\bfm -> 
                (BFOk , if (cells bfm !! headpos bfm) == bfZero (bfarch bfm) 
                        then bfm 
                        else let newprog = BFProgram (bfprog ++ [BFLoop (BFProgram bfprog)]) in
                        case (bfstack bfm) of
                            [] -> bfm { bfstack = [newprog] }
                            ((BFProgram []):progs) -> bfm { bfstack = newprog:progs }
                            progs -> bfm { bfstack = newprog:progs }))
            BFBreak n -> state (\bfm -> (BFPause n, bfm))

runNextCommand :: Eq cell => BFMachMonad cell buf BFResult
runNextCommand
    = state (\bfm ->
        let arch = bfarch bfm in
        case (bfstack bfm) of
            [] -> (BFError cEND_OF_CODE_MSG, bfm)
            ((BFProgram []):progs) -> (BFOk, bfm { bfstack = progs })
            ((BFProgram (cmd:cmds)):progs) 
                -> S.runState (runCommand cmd) (bfm { bfstack = (BFProgram cmds):progs }))

isPause :: BFResult -> Bool
isPause (BFPause n) = True
isPause _           = False

debuggerLoadBufIn :: BFDebugMonad cell buf ()
debuggerLoadBufIn
    = do
        inputBuffer <- fmap (bufin . bfmach) get
        inputString <- fmap bfinput get
        bufInInt <- fmap (bufInInterface . bfarch . bfmach) get
        let (inputBuffer', inputString') = bufInInt inputBuffer inputString
        modify (\bfdb -> bfdb { bfmach = (bfmach bfdb) { bufin = inputBuffer' }, bfinput = inputString' })
        return () 

debuggerStep :: Eq cell => BFDebugMonad cell buf ()
debuggerStep
  = do
      debuggerLoadBufIn
      state (\bfdb ->
        case (bfstatus bfdb) of
            BFError _ -> ((), bfdb)
            _ ->
                let bfm = bfmach bfdb in
                let (stat, bfm') = S.runState runNextCommand bfm in
                let newdbg = if isPause stat then DebugStepping else debugstate bfdb in
                let (newbufout, outstr) = (bufOutInterface $ bfarch bfm') (bufout bfm') in
                let newout = bfoutput bfdb ++ outstr in
                ((), bfdb { 
                    bfmach = bfm' { bufout = newbufout }, 
                    bfstatus = stat, 
                    debugstate = newdbg,
                    ticks = 1,
                    bfoutput = newout 
                }))

debuggerJump :: Eq cell => BFDebugMonad cell buf ()
debuggerJump
    = do
        debuggerStep
        bfdb <- get
        case (bfstatus bfdb) of
            BFPause _ -> return ()
            BFError _ -> return ()
            _ -> debuggerJump

debuggerJumpTo :: Eq cell => Int -> BFDebugMonad cell buf ()
debuggerJumpTo n
    = do
        debuggerStep
        bfdb <- get
        if (bfstatus bfdb) == BFPause n
            then return ()
            else debuggerJumpTo n

