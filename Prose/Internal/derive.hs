#!/usr/bin/env runhaskell
module Main where               -- stupid haskell-mode

import System.Environment
import Text.Parsec
import Text.Printf
import Data.List
import Data.Char
import Data.Maybe

main = do [file,name] <- getArgs
          s <- readFile file
          writeFile (name++".hs") (conv name s)

data Pat = Pat String

conv name = (prelude name ++) . (++coda) . intercalate ",\n" . map ("\t"++) . catMaybes . map (convLine name) . lines

prelude name = unlines [ "-- generated",
                         "module Prose.Internal."++name++" where",
                         (map toLower name) ++ " = [" ]
coda = " ]"


convLine :: String -> String -> Maybe String
convLine name s =
    case parse (line name) "" s of
               Right (Left x)      -> Just $ printf "['\\x%s']" x
               Right (Right (x,y)) -> Just $ printf "['\\x%s'..'\\x%s']" x y
               Left _ -> Nothing


line :: String -> Parsec String () (Either String (String,String))
line name = try (do { x <- point; string ".."; y <- point; isName; return (Right (x,y)) })
             <|> do { x <- point; isName; return (Left x) }
    where isName = spaces >> string "; " >> string name


point = many1 hexDigit

