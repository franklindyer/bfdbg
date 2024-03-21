module BFArchitectures where

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

-- -- -- -- -- -- --
-- ARCHITECTURES  --
-- -- -- -- -- -- --

-- 8-bit cells supporting both overflow and underflow
bf256ou :: BFArchitecture Int (Maybe Int)
bf256ou = BFArchitecture {
    archName = "BF256OU",
    archNameVerbose = "BF with 8-bit over- and underflowing cells",
    bfZero = 0,
    bfEmptyBuf = Nothing,
    incrementCell = Just . \c -> mod (c + 1) 256,
    decrementCell = Just . \c -> mod (c - 1) 256,
    readToCell = \b -> (Nothing, maybe 0 id b),
    writeFromCell = \b -> Just,
    bufInInterface = \b -> \s -> maybe (if null s then (Nothing, "") else (Just $ ord $ head s, tail s)) (\b' -> (Just b', s)) b,
    bufOutInterface = \b -> (Nothing, maybeToList $ fmap chr b)
}

bfNat :: BFArchitecture Integer (Maybe Integer)
bfNat = BFArchitecture {
    archName = "BFNAT",
    archNameVerbose = "BF with bounded below and unbounded above natural number cells",
    bfZero = 0,
    bfEmptyBuf = Nothing,
    incrementCell = Just . (1+),
    decrementCell = \c -> if c > 0 then Just (c-1) else Nothing,
    readToCell = \b -> (Nothing, maybe 0 id b),
    writeFromCell = \b -> Just,
    bufInInterface = undefined,
    bufOutInterface = \b -> (Nothing, concat $ maybeToList $ fmap show b)
}

