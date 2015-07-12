{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Prose.Segmentation.Words where

import Prose.CharSet as CSet hiding (map,filter)
import Prose.CharSet (CharSet)
import qualified Data.CharSet.Unicode as Unicode
import Prose.Segmentation.Common
import Data.Attoparsec.Text hiding (Result)
import Data.Attoparsec.Combinator
import Control.Applicative
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Function
import Data.Either (rights)
import Data.Traversable
import Data.Ord (comparing)
import Data.Monoid
import Control.Monad
import qualified Data.Attoparsec.Internal.Types as A
import Prose.Internal.Missings (hebrewLetter,numeric,aLetter,
                                midNum,katakana,graphemeExtend)

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

data Rule a = SimpleRule a | ExtendedRule a

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


aux = extend ∪ Unicode.format
    where format = Unicode.format ∖ (⊙)['\x200B','\x200C','\x200D']
          extend = graphemeExtend ∪ Unicode.spacingCombiningMark

auxRule = charOf aux

sep = (⊙)['\x0085','\x2028','\x2029']

getPos :: Parser Int
getPos = A.Parser $ \t pos more _ succ -> succ t pos more (A.fromPos pos)



data Result = Break | Don'tBreak deriving (Show,Eq)

isBreak :: [Result] -> Bool
isBreak brks | (Break:_) <- brks = True
             | otherwise         = False


split :: Text -> [Int] -> [Text]
split txt [] = [txt]
split txt (s:ss) = t : split rest (map (subtract s) ss)
    where (t,rest) = T.splitAt s txt

segment = map T.unpack . segmentT . T.pack

segmentT str = split str
             . map fst
             . filter snd
             . map (second isBreak)
             . map (\gr@((r,i):_) -> (i, map fst gr))
             . groupBy ((==) `on` snd)
             $ concat [ map (fmap (+i)) $ look str'
                            | (i,str') <- zip [(0::Int)..] (T.tails str) ]


look str = rights . map (lookRule str) $ rulesWord

lookRule str rule = parseOnly (toParser rule) str
    where toParser0 f (a :× b) = (Don'tBreak,) <$> parserOf (lsParser f a) (lsParser f b)
          toParser0 f (a :÷ b) = (Break,)      <$> parserOf (lsParser f a) (lsParser f b)

          toParser (SimpleRule r)   = toParser0 simParser r
          toParser (ExtendedRule r) = toParser0 augParser r

          parserOf pa pb = pa *> lookAhead pb *> getPos

          lsParser f = foldr1 (*>) . map f

          simParser c = charOf c
--          augParser' c = (charOf c <* auxRule) <|> charOf c
          augParser c = (charOf c <* many auxRule)


--outcomeAt str = [ parseOnly r <- rulesWord ]


