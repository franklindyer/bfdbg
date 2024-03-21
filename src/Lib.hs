module Lib
    ( someFunc
    ) where

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

data Void
data TickerEvent = TickerEvent

data BFCommand = BFLeft | BFRight | BFPlus | BFMinus | BFGet | BFPut | BFLoop BFProgram | BFBreak Int
    deriving Show

data BFProgram = BFProgram [BFCommand] deriving Show

data BFArchitecture cell buf = BFArchitecture {
    bfZero          :: cell,
    bfEmptyBuf      :: buf,
    incrementCell   :: cell -> Maybe cell,
    decrementCell   :: cell -> Maybe cell,
    readToCell      :: buf -> (buf, cell),
    writeFromCell   :: buf -> cell -> buf,
    bufInInterface  :: buf -> (buf, Bool),      -- Bool indicates whether more input is needed
    bufOutInterface :: buf -> (buf, Maybe String)
}

data BFResult = BFOk | BFError String | BFPause Int deriving (Eq, Show)

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

data BFDebugState = DebugRunning | DebugStepping deriving (Eq, Show)

data BFDebugSettings = BFDebugSettings {
    msPerStep :: Int
}

defaultDebugSettings = BFDebugSettings {
    msPerStep = 1024
}

data BFDebugger cell buf = BFDebugger {
    bfmach :: BFMachine cell buf,
    bfstatus :: BFResult,
    debugstate :: BFDebugState,
    bfoutput :: String,
    ticks :: Int,
    debugconf :: BFDebugSettings
}

type BFDebugMonad cell buf = S.State (BFDebugger cell buf)

-- -- -- -- --
-- PARSING  --
-- -- -- -- --

bfParsePrimitive :: Parsec String Int BFCommand
bfParsePrimitive
    = (char '<' >> return BFLeft)
        <|> (char '>' >> return BFRight)
        <|> (char '+' >> return BFPlus)
        <|> (char '-' >> return BFMinus)
        <|> (char '.' >> return BFPut)
        <|> (char ',' >> return BFGet)
        <|> (char '@' >> do { n <- getState; modifyState (+1); return (BFBreak n)})

bfParser :: Parsec String Int BFProgram
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

debuggerStep :: Eq cell => BFDebugMonad cell buf ()
debuggerStep
    = state (\bfdb ->
        case (bfstatus bfdb) of
            BFError _ -> ((), bfdb)
            _ ->
                let bfm = bfmach bfdb in
                let (stat, bfm') = S.runState runNextCommand bfm in
                let newdbg = if isPause stat then DebugStepping else debugstate bfdb in
                let (newbufout, outstr) = (bufOutInterface $ bfarch bfm') (bufout bfm') in
                let newout = bfoutput bfdb ++ (maybe [] id outstr) in
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
            bfShowMemPane bfvs bfdb
            <=> hBorder 
            <=> (bfShowOutPane bfvs bfdb))
        <=> bfShowMemInfoPane bfvs bfdb
        <=> bfShowDbgPane bfvs bfdb 
        <=> border bfInstructions
    ]

-- -- -- -- -- --
-- CONTROLLER  --
-- -- -- -- -- --

isStepTime :: BFDebugger cell buf -> Bool
isStepTime bfdb = mod (ticks bfdb) (msPerStep $ debugconf bfdb) == 0

stepDurIncrease :: BFDebugger cell buf -> BFDebugger cell buf
stepDurIncrease bfdb
    = bfdb { debugconf = dbc { msPerStep = min 4096 (msPerStep dbc * 2) }, debugstate = DebugStepping }
        where dbc = debugconf bfdb

stepDurDecrease :: BFDebugger cell buf -> BFDebugger cell buf
stepDurDecrease bfdb
    = bfdb { debugconf = dbc { msPerStep = max 1 (msPerStep dbc `div` 2) }, debugstate = DebugStepping }
        where dbc = debugconf bfdb

bfAppEvent :: 
    Eq cell => BrickEvent () TickerEvent -> EventM () (BFDebugger cell buf) ()
bfAppEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) 
            -> halt
        VtyEvent (V.EvKey V.KDown []) 
            -> state (\bfdb -> ((), stepDurIncrease bfdb))
        VtyEvent (V.EvKey V.KUp []) 
            -> state (\bfdb -> ((), stepDurDecrease bfdb))
        VtyEvent (V.EvKey (V.KChar 'r') []) 
            -> state (\bfdb -> ((), bfdb { debugstate = DebugRunning }))
        VtyEvent (V.EvKey (V.KChar 's') []) 
            -> state (\bfdb -> S.runState debuggerStep (bfdb { debugstate = DebugStepping }))
        VtyEvent (V.EvKey (V.KChar 'j') []) 
            -> state (\bfdb -> S.runState debuggerJump bfdb)
        VtyEvent _ -> state (\bfdb -> ((), bfdb))
        AppEvent TickerEvent -> state (\bfdb ->  
            if debugstate bfdb == DebugRunning && isStepTime bfdb
            then S.runState debuggerStep bfdb
            else ((), bfdb { ticks = 1 + ticks bfdb }))

bfMakeApp :: 
    Eq cell => BFViewSettings cell -> App (BFDebugger cell buf) TickerEvent ()
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

-- 8-bit cells supporting both overflow and underflow
bf256ou :: BFArchitecture Int (Maybe Int)
bf256ou = BFArchitecture {
    bfZero = 0,
    bfEmptyBuf = Nothing,
    incrementCell = Just . \c -> mod (c + 1) 256,
    decrementCell = Just . \c -> mod (c - 1) 256,
    readToCell = \b -> (Nothing, maybe 0 id b),
    writeFromCell = \b -> Just,
    bufInInterface = undefined,
    bufOutInterface = \b -> (Nothing, fmap ((:[]) . chr) b)
}

bfNat :: BFArchitecture Integer (Maybe Integer)
bfNat = BFArchitecture {
    bfZero = 0,
    bfEmptyBuf = Nothing,
    incrementCell = Just . (1+),
    decrementCell = \c -> if c > 0 then Just (c-1) else Nothing,
    readToCell = \b -> (Nothing, maybe 0 id b),
    writeFromCell = \b -> Just,
    bufInInterface = undefined,
    bufOutInterface = \b -> (Nothing, fmap show b)
}

-- -- -- --
-- MAIN  --
-- -- -- -- 

myShow :: Int -> String
myShow x = printf "%02x" x

-- bfprog = "+>++>@+++>@++++++++++++++++@[->+<@]"
-- bfprog = "++++-----<"
bfprog = "+[[->+++<]>@]"
bfparsed = either (\_ -> BFProgram []) id (runParser bfParser 0 "" bfprog)

someFunc :: IO ()
someFunc
    = do
        let bfm = (bfInitMachine bfNat 100) { bfstack = [bfparsed] }
        let bfvs = BFViewSettings {showCell = show, cellSpacing = 0}
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

        void $ customMainWithDefaultVty (Just chan) (bfMakeApp bfvs) bfdb
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
