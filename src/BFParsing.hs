module BFParsing where

import Data.Char
import Data.List.Split
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import qualified Graphics.Vty as V

import BFTypes

-- -- -- -- --
-- PARSING  --
-- -- -- -- --

bfParseRunOpts  :: String -> (String, String, String)
bfParseRunOpts opts
    = let optparts = splitOn " " opts in
        (optparts !! 0, optparts !! 1, optparts !! 2)

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

-- -- -- -- -- --
-- TRANSPILING --
-- -- -- -- -- --

bf256ouTo2ou :: String -> String
bf256ouTo2ou = (initializer ++) . concatMap subber
    where
        initializer = "+"
        plusSub = ">[+>]+<+[+<+]+>>>>>>>>>>[+]+<[>+<+]<<<<<<<<<"
        minusNoFlag = "+>+[>+]<[<]+"
        minusSub = ">>>>>>>>>[+]+<<<<<<<<<" ++ minusNoFlag ++ minusNoFlag ++ ">>>>>>>>>[+]<<<<<<<<<" ++ plusSub
        leftSub = "<<<<<<<<<<<"
        rightSub = ">>>>>>>>>>>[+]+"
        putSub = ">.>.>.>.>.>.>.>.<<<<<<<<"
        getSub = ">,>,>,>,>,>,>,>,<<<<<<<<"
        openLoopSub = ">>>>>>>>>>[<<<<<<<<<<"
        closeLoopSub = ">>>>>>>>>>]<<<<<<<<<<"
        subber c = case c of
            '+' -> plusSub
            '-' -> minusSub
            '<' -> leftSub
            '>' -> rightSub
            '.' -> putSub
            ',' -> getSub
            '[' -> openLoopSub
            ']' -> closeLoopSub
            '@' -> "@"
            _   -> ""

bf256ouTo2ouTranspiler :: String -> String
bf256ouTo2ouTranspiler incode = outcode
    where
        (header, cmds) = (takeWhile (/= '\n') incode, tail $ dropWhile (/= '\n') incode)
        (inarch, memsize, infile) = bfParseRunOpts header
        newMemsize = 11 * (read memsize :: Int)
        outcode = "bf2ou " ++ show newMemsize ++ " " ++ infile ++ "\n" ++ bf256ouTo2ou cmds
