module BFParsing where

import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import qualified Graphics.Vty as V

import BFTypes

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
