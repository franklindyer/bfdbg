module Lib
    ( someFunc
    ) where

import Control.Lens
import Control.Monad.State.Lazy as S
import Data.Char
import System.IO
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

data BFCommand = BFLeft | BFRight | BFPlus | BFMinus | BFGet | BFPut | BFLoop BFProgram deriving Show

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

data BFMachine cell buf = BFMachine {
    bfarch  :: BFArchitecture cell buf,
    time    :: Int,
    ncells  :: Int,
    headpos :: Int,
    cells   :: [cell],
    bufin   :: buf,
    bufout  :: buf,
    bfstack :: [BFProgram]
}

data BFStatus = BFError String | BFOk

type BFMachMonad cell buf = S.State (BFMachine cell buf)

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

runCommand :: Eq cell => BFArchitecture cell buf -> BFCommand -> BFMachMonad cell buf BFStatus
runCommand arch cmd
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
                    (incrementCell arch (cells bfm !! headpos bfm))) 
            BFMinus -> state (\bfm ->
                maybe
                    (BFError cNO_DEC_ERRORMSG , bfm)
                    (\newc -> (BFOk , bfm { cells = cells bfm & element (headpos bfm) .~ newc }))
                    (decrementCell arch (cells bfm !! headpos bfm)))
            BFGet -> state (\bfm ->
                let (buf' , newc) = readToCell arch $ bufin bfm in
                (BFOk , bfm { bufin = buf', cells = cells bfm & element (headpos bfm) .~ newc }))
            BFPut -> state (\bfm ->
                (BFOk , bfm { bufout = writeFromCell arch (bufout bfm) (cells bfm !! headpos bfm) }))
            BFLoop bfprog -> state (\bfm -> 
                (BFOk , if (cells bfm !! headpos bfm) == bfZero arch 
                        then bfm 
                        else bfm { bfstack = bfprog:(bfstack bfm) } ))

-- -- -- -- -- --
-- DISPLAYING  --
-- -- -- -- -- --

data BFViewSettings cell = BFViewSettings {
    showCell    :: cell -> String,
    cellSpacing :: Int
}

bfShowMach :: BFViewSettings cell -> BFMachine cell buf -> String
bfShowMach conf bfm = foldr (\x y -> showCell conf x ++ ' ':y) "" (cells bfm)

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

bfprog = "+>++>+++>++++++++++++++++"
bfparsed = either (\_ -> BFProgram []) id (parse bfParser "" bfprog)

someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
someFunc 
    = do
        putStrLn $ show bfparsed
        let bfm = bfInitMachine bf256ou 10
        go bfparsed bfm
        return ()
    where
        bfv = bfShowMach (BFViewSettings {showCell = myShow, cellSpacing = 0})
        go :: BFProgram -> BFMachine Int (Maybe Int) -> IO (BFProgram, BFMachine Int (Maybe Int))
        go (BFProgram []) bfm = return (BFProgram [], bfm)
        go (BFProgram (cmd:cmds)) bfm
            = do
               let (_,bfm') = runState (runCommand bf256ou cmd) bfm
               putStrLn $ bfv bfm'
               go (BFProgram cmds) bfm' 
