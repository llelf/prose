-- |
-- Module      : Prose.Segmentation.Words
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--


{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Prose.Segmentation.Words where

import Prose.CharSet as CSet
import Prose.CharSet (CharSet)
import qualified Data.CharSet.Unicode as Unicode
import Prose.Segmentation.Common
import Data.Attoparsec.Text hiding (Result)
import Data.Attoparsec.Combinator
import Control.Applicative
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Data.List
import Data.Function
import Data.Either (rights)
import Data.Ord (comparing)
import Data.Monoid
import Control.Monad
import qualified Data.Attoparsec.Internal.Types as A
import Prose.Internal.Missings (hebrewLetter,numeric,aLetter,
                                midNum,katakana,graphemeExtend)


-- UAX#29, Unicode 8.0.0

data RuleTerm = [CharSet] :× [CharSet]  -- do not break
              | [CharSet] :÷ [CharSet]  -- do break
infix 0 :×,:÷


newline = (⊙)['\x000B','\x000C','\x0085','\x2028','\x2029']

midNumLet = (⊙)['\x002E','\x2018','\x2019','\x2024','\xFE52','\xFF07','\xFF0E']
midLetter = (⊙)['\x00B7','\x0387','\x05F4','\x2027','\x003A',
                '\xFE13','\xFE55','\xFF1A','\x02D7']

singleQuote = CSet.singleton '\x0027'
doubleQuote = CSet.singleton '\x0022'

ahLetter = aLetter ∪ hebrewLetter
midNumLetQ = midNumLet ∪ singleQuote

extendNumLet = Unicode.connectorPunctuation
regionalIndicator = (⊙)['\x1F1E6'..'\x1F1FF']

charOf :: CharSet -> Parser Char
charOf = satisfy . flip CSet.member


{-
data ExtendedParser a = ExtendedParser { unExtendedParser :: Parser a,
                                                      aux :: Parser a }
    deriving Functor
         
extend :: Parser Char -> ExtendedParser Char
extend p = ExtendedParser p (char 'x')


instance Applicative (ExtendedParser) where
    ExtendedParser f m <*> ExtendedParser p n = ExtendedParser (f <*> (n *> p *> n)) (m<*>n)
--    ExtendedParser f m *> ExtendedParser p n  = ExtendedParser (f *> n *> p *> n) n
    pure v = ExtendedParser (pure v) (aux (pure v))
-}

-- UAX#29 4.1.1

data Rule a = SimpleRule a
            | ExtendedRule a    -- those have to be extended as per UAX#29:6.2 (see rule WB4 below)


rulesSimple = map SimpleRule [
                    --          sot :÷                                     -- WB1
                    --              :÷ eot                                 -- WB2
                               [cr] :× [lf],                               -- WB3li
                [newline ∪ cr ∪ lf] :÷ [whatever],                         -- WB3a
                         [whatever] :÷ [newline ∪ cr ∪ lf],                -- WB3b
                           [(¬)sep] :× [aux]                               -- WB4   X (Extend ∪ Format)* → X
 ]
rulesExtended = map ExtendedRule [
                         [ahLetter] :× [ahLetter],                         -- WB5
                         [ahLetter] :× [midLetter ∪ midNumLetQ, ahLetter], -- WB6
 [ahLetter, midLetter ∪ midNumLetQ] :× [ahLetter],                         -- WB7
                     [hebrewLetter] :× [singleQuote],                      -- WB7a     
                     [hebrewLetter] :× [doubleQuote, hebrewLetter],        -- WB7b
        [hebrewLetter, doubleQuote] :× [hebrewLetter],                     -- WB7c
                          [numeric] :× [numeric],                          -- WB8
                         [ahLetter] :× [numeric],                          -- WB9
                          [numeric] :× [ahLetter],                         -- WB10
     [numeric, midNum ∪ midNumLetQ] :× [numeric],                          -- WB11
                          [numeric] :× [midNum ∪ midNumLetQ, numeric],     -- WB12
                         [katakana] :× [katakana],                         -- WB13
   [ahLetter ∪ numeric
         ∪ katakana ∪ extendNumLet] :× [extendNumLet],                     -- WB13a
                     [extendNumLet] :× [ahLetter ∪ numeric ∪ katakana],    -- WB13b
                [regionalIndicator] :× [regionalIndicator],                -- WB13c
                         [whatever] :÷ [whatever]                          -- WB14
 ]



rulesWord = rulesSimple <> rulesExtended


aux = extend ∪ format
    where format = Unicode.format ∖ (⊙)['\x200B','\x200C','\x200D']
          extend = graphemeExtend ∪ Unicode.spacingCombiningMark

auxRule = charOf aux

sep = (⊙)['\x0085','\x2028','\x2029']

-- | Current parser position (in 16-bit words, NOT in code points).
getPos :: Parser Int
getPos = A.Parser $ \t pos more _ succ -> succ t pos more (A.fromPos pos)



data Result = Break | Don'tBreak deriving (Show,Eq)

isBreak :: [Result] -> Bool
isBreak brks | (Break:_) <- brks = True
             | otherwise         = False


-- split :: Text -> [Int] -> [Text]
-- split txt [] = [txt]
-- split txt (s:ss) = t : split rest (map (subtract s) ss)
--     where (t,rest) = T.splitAt s txt

-- | >>> split "abc🐧" [1,3]
-- ["a","bc","\128039"]
split :: Text -> [Int] -> [Text]
split t ss = split0 t (0:ss)

split0 txt@(TI.Text a _ end) [s] = [TI.Text a s (end-s)]
split0 txt@(TI.Text a _ _) (s1:s2:ss) = t : split0 txt (s2:ss)
    where t = TI.Text a s1 (s2-s1)


segment :: [Char] -> [[Char]]
segment = map T.unpack . segmentT . T.pack

segmentT :: Text -> [Text]
segmentT str = split str                                         -- split on
             . map fst                                           -- indeces where there’s a break
             . filter snd
             . map (second isBreak)                              -- (Int, break here? :: Bool)
             . map (\gr@((_,i):_) -> (i, map fst gr))            -- (Int, results in order :: [Result])
             . groupBy ((==) `on` snd)
             . sortBy (comparing snd)                            -- sort and group on
             $ concat [ map (fmap (+i)) $ look str'              -- stream of (Result,Int)
                                                                 -- (not necessary in order)
                        | str' @(TI.Text _ i _) <- T.tails str ] -- going code point by code point

-- | Results of all rules that’s matched (at the beginning of the string)
look str = rights . map (lookRule str) $ rulesWord


-- | Apply the specific rule at the beginning of the string
lookRule :: Text -> Rule RuleTerm -> Either String (Result,Int)
lookRule str rule = parseOnly (toParser rule) str
    where toParser0 f (a :× b) = (Don'tBreak,) <$> parserOf (lsParser f a) (lsParser f b)
          toParser0 f (a :÷ b) = (Break,)      <$> parserOf (lsParser f a) (lsParser f b)

          toParser (SimpleRule r)   = toParser0 simParser r
          toParser (ExtendedRule r) = toParser0 augParser r

          parserOf pa pb = pa *> lookAhead pb *> getPos

          lsParser f = foldr1 (*>) . map f

          simParser c = charOf c
          augParser c = charOf c <* many auxRule -- WB4


