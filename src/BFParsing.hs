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

{-
    A cell consists of a statically sized number of bits.
    A cell group consists of a dynamically sized number of cells, representing a single Nat-sized cell.
    The format of a cell is as follows:
    - 1 "initialized" bit
    - A static number of data bits
    - 1 "overflow" bit
    - A statically sized metadata block
        - 1 "this is not the first cell in the group" bit
        - 1 "this cell is currently being expanded" bit
        - 1 "this cell (not cell group!) contains a nonzero value" bit (will need to update this with each increment and decrement operation)
-}
bfNatTo2ouTranspiler :: String -> String
bfNatTo2ouTranspiler incode = outcode
    where
    (header, cmds) = (takeWhile (/= '\n') incode, tail $ dropWhile (/= '\n') incode)
    (inarch, memsize, infile) = bfParseRunOpts header
    newMemsize = 20 * (read memsize :: Int)
    outcode = "bf2ou " ++ show newMemsize ++ " " ++ infile ++ "\n" ++ initNewCell ++ concatMap subber cmds
    dataSize = 4
    cellSize = dataSize + 2 + 5
    initNewCell = "+"
    initNewGroup = initNewCell
    panicOOB = "[+]+[<[+]+]"
    succCell = ">[+>]+<+[+<+]+"
    moveCellRight = replicate cellSize '>'
    moveCellLeft = replicate cellSize '<'
    moveToMetadata = replicate (dataSize+2) '>'
    moveFromMetadata = replicate (dataSize+2) '<'
    moveGroupRight = moveToMetadata ++ "[" ++ moveCellRight ++ "]" ++ moveFromMetadata
    moveGroupLeft = moveToMetadata ++ moveCellLeft ++ "[" ++ moveCellLeft ++ "]" ++ moveFromMetadata
    findEmpty = "[" ++ moveCellRight ++ "]"
    copyCellRight = concat (replicate cellSize ("[" ++ moveCellRight ++ "+" ++ moveCellLeft ++ "+]>")) ++ moveCellLeft 
    bubbleNewCell 
        = moveToMetadata ++ ">+<" ++ moveFromMetadata ++        -- Set the "this group is being expanded" bit
            findEmpty ++ moveCellLeft ++                        -- Go to the edge of currently initialized memory
            moveToMetadata ++ ">+[+<" ++                          -- Open loop: this will continue until arriving at the cell being expanded
            moveFromMetadata ++ copyCellRight ++ moveCellLeft ++ moveToMetadata ++
            ">+]<" ++ moveFromMetadata ++                        -- Close loop: we have arrived at the cell group that needs expanding and deactivated its flag
            moveCellRight ++ initNewCell ++                      -- Initialize the new cell in the group
            moveToMetadata ++ "+" ++ moveFromMetadata      -- Activate the "not the first cell" bit of the new cell
    goGroupHome = moveToMetadata ++ "[" ++ moveCellLeft ++ "]" ++ moveFromMetadata
    succGroup
        = succCell ++ moveToMetadata ++ "<[+>" ++ moveCellRight ++ "+[+" ++ moveCellLeft ++ moveFromMetadata ++ bubbleNewCell ++ moveToMetadata ++ "[+]+" ++ "+]+" ++ moveFromMetadata ++ succCell ++ moveToMetadata ++ "<]>" ++ moveFromMetadata ++ goGroupHome 
    subber c = case c of
        '+' -> succGroup
        _   -> ""

locateTranspiler :: String -> String -> Maybe (String -> String)
locateTranspiler "bf256ou" "bf2ou"  = Just bf256ouTo2ouTranspiler
locateTranspiler "bfNat" "bf2ou"    = Just bfNatTo2ouTranspiler
locateTranspiler _ _                = Nothing
