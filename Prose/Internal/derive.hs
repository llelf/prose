#!/usr/bin/env runhaskell

module Main where               -- stupid haskell-mode

import System.Environment
import Text.Parsec
import Text.Printf
import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

main = do [act,file,name] <- getArgs
          s <- readFile file
          writeFile (name++".hs") (conv ((Map.!) actions act) name s)

actions = Map.fromList [("set",        convLine),
                        ("break-test", convLine1),
                        ("norm-test",  convNormTest)]


conv convLine name =
      (prelude name ++)
    . (++coda)
    . intercalate ",\n"
    . map ("\t"++) . catMaybes . map (convLine name) . lines

prelude name = unlines [ "-- generated",
                         "module Prose.Internal."++name++" where",
                         (map toLower name) ++ " = [" ]
coda = " ]\n"


convLine :: String -> String -> Maybe String
convLine name s =
    case parse (line name) "" s of
               Right (Left x)      -> Just $ printf "[%s]" (xpoint x)
               Right (Right (x,y)) -> Just $ printf "[%s..%s]" (xpoint x) (xpoint y)
               Left _ -> Nothing


line :: String -> Parsec String () (Either String (String,String))
line name = try (do { x <- point; string ".."; y <- point; isName; return (Right (x,y)) })
             <|> do { x <- point; isName; return (Left x) }
    where isName = spaces >> string "; " >> string name >> space

convLine1 :: String -> String -> Maybe String
convLine1 _ s | "#" `isPrefixOf` s = Nothing
              | otherwise          = Just $ show ss
    where ss = map (map point2c) . map (filter(/="×")) . wordsBy (=="÷") . takeWhile (/="#") . words $ s



convNormTest :: String -> String -> Maybe String
convNormTest _ s | "#" `isPrefixOf` s     = Nothing
                 | "@Part" `isPrefixOf` s = Nothing
                 | otherwise              = Just $ show ss
    where ss = map (map point2c . words) . take 5 . wordsBy (==';') $ s

point = many1 hexDigit

xpoint = printf "'\\x%s'" :: String -> String
point2c = read . xpoint :: String -> Char


