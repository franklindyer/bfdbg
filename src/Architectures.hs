module Architectures where

import Data.Char
import Lib

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
    bufInInterface = undefined,
    bufOutInterface = \b -> (Nothing, fmap ((:[]) . chr) b)
}

-- Unbounded-above natural number cells
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
    bufOutInterface = \b -> (Nothing, fmap show b)
}
