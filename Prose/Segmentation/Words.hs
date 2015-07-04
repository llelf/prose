{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Prose.Segmentation.Words where

import Prose.CharSet as CSet hiding (map)
import Prose.CharSet (CharSet)
import qualified Data.CharSet.Unicode as Unicode
import Prose.Segmentation.Common
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import qualified Data.Text as T
import Data.List
import Data.Either (rights)
import Data.Traversable
import Data.Ord (comparing)
import qualified Data.Attoparsec.Internal.Types as A
import Prose.Internal.Missings (hebrewLetter,numeric,aLetter,
                                midNum,katakana)

data Rule = [CharSet] :× [CharSet]  -- do not break
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


rulesWord = [
                    --          sot :÷                                     -- WB1
                    --              :÷ eot                                 -- WB2
                               [cr] :× [lf],                               -- WB3li
                [newline ∪ cr ∪ lf] :÷ [whatever],                         -- WB3a
                         [whatever] :÷ [newline ∪ cr ∪ lf],                -- WB3b
      -- WB4   X (Extend ∪ Format)*  →  X
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


getPos :: Parser Int
getPos = A.Parser $ \t pos more _ succ -> succ t pos more (A.fromPos pos)


data Result = Break | Don'tBreak deriving Show

breaks str = --sortBy (comparing snd) $
             concat [ rights . map ((fmap.fmap) (+i) . flip parseOnly str') . map toParser $ rulesWord
                      | (i,str') <- zip [(0::Int)..] (T.tails str) ]
    where toParser (a :× b) = (Don'tBreak,) <$> parserOf a b
          toParser (a :÷ b) = (Break,)      <$> parserOf a b

          parserOf [a] [b] = charOf a *> lookAhead (charOf b) *> getPos
          parserOf _ _ = fail ""

