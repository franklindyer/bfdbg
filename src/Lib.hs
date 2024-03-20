module Lib
    ( someFunc
    ) where

import Brick
import Control.Lens
import Control.Monad (void, forever)
import Control.Monad.State.Strict
import Control.Monad.State.Lazy as S
import Data.Char
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import qualified Graphics.Vty as V

data Void

data BFCommand = BFLeft | BFRight | BFPlus | BFMinus | BFGet | BFPut | BFLoop BFProgram | BFBreak 
    deriving Show

data BFProgram = BFProgram [BFCommand] deriving Show

data BFArchitecture cell buf = BFArchitecture {
    bfZero          :: cell,
    bfEmptyBuf      :: buf,
    incrementCell   :: cell -> Maybe cell,
    decrementCell   :: cell -> Maybe cell,
    readToCell      :: buf -> (buf, cell),
    writeFromCell   :: buf -> cell -> buf,
    bufInInterface  :: buf -> Handle -> IO buf,
    bufOutInterface :: buf -> Handle -> IO buf
}

data BFStatus = BFError String | BFOk deriving (Eq, Show)

data BFMachine cell buf = BFMachine {
    bfarch      :: BFArchitecture cell buf,
    time        :: Int,
    ncells      :: Int,
    headpos     :: Int,
    cells       :: [cell],
    bufin       :: buf,
    bufout      :: buf,
    bfstack     :: [BFProgram]
}

type BFMachMonad cell buf = S.State (BFMachine cell buf)

data BFDebugState = DebugPaused | DebugRunning | DebugJumping | DebugStepping

data BFDebugger cell buf = BFDebugger {
    bfmach :: BFMachine cell buf,
    bfstatus :: BFStatus,
    debugstate :: BFDebugState,
    bfoutput :: String
}

type BFDebugMonad cell buf = S.State (BFDebugger cell buf)

-- -- -- -- --
-- PARSING  --
-- -- -- -- --

bfParsePrimitive :: Parsec String st BFCommand
bfParsePrimitive
    = (char '<' >> return BFLeft)
        <|> (char '>' >> return BFRight)
        <|> (char '+' >> return BFPlus)
        <|> (char '-' >> return BFMinus)
        <|> (char '.' >> return BFGet)
        <|> (char ',' >> return BFPut)

bfParser :: Parsec String st BFProgram
bfParser
    = fmap BFProgram $ many $
      bfParsePrimitive
        <|> (fmap BFLoop $ between (char '[') (char ']') bfParser)

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

bfRequire :: String -> Bool -> BFStatus
bfRequire msg cond = if cond then BFOk else BFError msg 

runCommand :: Eq cell => BFCommand -> BFMachMonad cell buf BFStatus
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

runNextCommand :: Eq cell => BFMachMonad cell buf BFStatus
runNextCommand
    = state (\bfm ->
        let arch = bfarch bfm in
        case (bfstack bfm) of
            [] -> (BFError cEND_OF_CODE_MSG, bfm)
            ((BFProgram []):progs) -> (BFOk, bfm { bfstack = progs })
            ((BFProgram (cmd:cmds)):progs) 
                -> S.runState (runCommand cmd) (bfm { bfstack = (BFProgram cmds):progs }))

debuggerStep :: Eq cell => BFDebugMonad cell buf ()
debuggerStep
    = state (\bfdb ->
        if bfstatus bfdb /= BFOk then ((), bfdb) else 
        let bfm = bfmach bfdb in
        let (stat, bfm') = S.runState runNextCommand bfm in
        ((), bfdb { bfmach = bfm', bfstatus = stat }))

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

bfUI :: BFViewSettings cell -> BFDebugger cell buf -> [Widget ()]
bfUI bfvs = (:[]) . (bfShowMemPane bfvs)

-- -- -- -- -- --
-- CONTROLLER  --
-- -- -- -- -- --

bfAppEvent :: 
    Eq cell => BrickEvent () Void -> EventM () (BFDebugger cell buf) ()
bfAppEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent _ -> state (\bfdb -> S.runState debuggerStep bfdb)

bfMakeApp :: 
    Eq cell => BFViewSettings cell -> App (BFDebugger cell buf) Void ()
bfMakeApp bfvs
    = App {
        appDraw = bfUI bfvs,
        appChooseCursor = showFirstCursor,
        appHandleEvent = bfAppEvent,
        appStartEvent = return (),
        appAttrMap = const $ attrMap V.defAttr []
      }

-- -- -- -- -- -- --
-- ARCHITECTURES  --
-- -- -- -- -- -- --

bf256ou :: BFArchitecture Int (Maybe Int)
bf256ou = BFArchitecture {
    bfZero = 0,
    bfEmptyBuf = Nothing,
    incrementCell = Just . \c -> mod (c + 1) 256,
    decrementCell = Just . \c -> mod (c - 1) 256,
    readToCell = \b -> (Nothing, maybe 0 id b),
    writeFromCell = \b -> Just,
    bufInInterface = \c -> \hdl -> maybe (fmap (Just . ord) $ hGetChar hdl) (return . Just) c,
    bufOutInterface = \c -> \hdl -> (maybe (return ()) (\c -> hPutChar hdl (chr c)) c) >> return Nothing
}

-- -- -- --
-- MAIN  --
-- -- -- -- 

myShow :: Int -> String
myShow x = printf "%02x" x

bfprog = "+>++>+++>++++++++++++++++[->+<]"
-- bfprog = "++++-----<"
bfparsed = either (\_ -> BFProgram []) id (parse bfParser "" bfprog)

someFunc :: IO ()
someFunc
    = do
        let bfm = (bfInitMachine bf256ou 30) { bfstack = [bfparsed] }
        let bfvs = BFViewSettings {showCell = myShow, cellSpacing = 0}
        let bfdb = BFDebugger {bfmach = bfm, bfstatus = BFOk, debugstate = DebugStepping, bfoutput = ""}
        void $ customMainWithDefaultVty Nothing (bfMakeApp bfvs) bfdb
-- someFunc = putStrLn "someFunc"
{-
someFunc 
    = do
        putStrLn $ show bfparsed
        let bfm = (bfInitMachine bf256ou 10) { bfstack = [bfparsed] }
        go bfm
        return ()
    where
        bfv = bfShowMem (BFViewSettings {showCell = myShow, cellSpacing = 0})
        go :: BFMachine Int (Maybe Int) -> IO (BFMachine Int (Maybe Int))
        go bfm
            = do
               let (bfmsg , bfm') = S.runState (runNextCommand bf256ou) bfm
               putStrLn $ bfv bfm'
               case bfmsg of
                BFOk -> go bfm'
                (BFError msg) -> putStrLn msg >> return bfm'
-}
