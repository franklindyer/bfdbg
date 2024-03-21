module BFController where

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
import BFInterpreter
import BFDisplay

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

