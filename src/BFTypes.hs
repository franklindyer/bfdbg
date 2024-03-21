module BFTypes where

import Control.Monad.State.Lazy as S

data Void
data TickerEvent = TickerEvent

data BFCommand = BFLeft | BFRight | BFPlus | BFMinus | BFGet | BFPut | BFLoop BFProgram | BFBreak Int
    deriving Show

data BFProgram = BFProgram [BFCommand] deriving Show

data BFArchitecture cell buf = BFArchitecture {
    archName        :: String,
    archNameVerbose :: String,
    bfZero          :: cell,
    bfEmptyBuf      :: buf,
    incrementCell   :: cell -> Maybe cell,
    decrementCell   :: cell -> Maybe cell,
    readToCell      :: buf -> (buf, cell),
    writeFromCell   :: buf -> cell -> buf,
    bufInInterface  :: buf -> String -> (buf, String),
    bufOutInterface :: buf -> (buf, String)
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
    bfinput :: String,
    bfoutput :: String,
    ticks :: Int,
    debugconf :: BFDebugSettings
}

type BFDebugMonad cell buf = S.State (BFDebugger cell buf)
