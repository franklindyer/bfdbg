module Lib
    ( someFunc
    ) where

import Data.Char
import System.IO
import Text.Parsec
import Text.Parsec.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BFCommand = BFLeft | BFRight | BFPlus | BFMinus | BFGet | BFPut | BFLoop BFProgram

data BFProgram = BFProgram [BFCommand]

data BFArchitecture cell buf = BFArchitecture {
    incrementCell   :: cell -> Maybe cell,
    decrementCell   :: cell -> Maybe cell,
    readToCell      :: buf -> Char -> (buf, cell),
    writeFromCell   :: buf -> cell -> (buf, Maybe Char)
}

data BFMachine cell buf = BFMachine {
    bfarch  :: BFArchitecture cell buf,
    ncells  :: Int,
    cells   :: [cell],
    buffer  :: buf
}

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

        
