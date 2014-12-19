-- | Grapheme Cluster Boundaries

{-# LANGUAGE TypeOperators #-}
module Prose.Segmentation.Graphemes where

import qualified Data.CharSet.Unicode as Unicode
import Data.Char
import Prose.CharSet as CSet hiding (map)
import Prose.CharSet (CharSet)
import Data.Monoid
import Data.List (groupBy)
import Data.Maybe

import Prose.Internal.Missings
import Prose.Types


-- Unicode 7.0.0 Annex #29

cr, lf :: CharSet
cr = CSet.singleton '\x000d'
lf = CSet.singleton '\x000a'

control :: CharSet
control = Unicode.lineSeparator
            ∪ Unicode.paragraphSeparator
            ∪ Unicode.control
            ∪ Unicode.notAssigned ∩ defaultIgnorableCodePoint
            ∪ Unicode.surrogate
            ∪ Unicode.format ∩ (⊙)['\xd', '\xa', '\x200c', '\x200d']

extend :: CharSet
extend = CSet.fromList [ '\x0300'..'\x036f' ] -- XXX WRONG

regionalIndicator :: CharSet
regionalIndicator = (⊙)[ '\x1F1E6'..'\x1F1FF' ]

prepend :: CharSet
prepend = CSet.empty

spacingMark = ()
    where exceptions = [ ]

l,v,t,lv,lvt :: CharSet
l = CSet.empty
v = CSet.empty
t = CSet.empty
lv = CSet.empty
lvt = CSet.empty

whatever :: CharSet
whatever = CSet.full


data Rule = CharSet :× CharSet  -- do break
          | CharSet :÷ CharSet  -- do not break

rules :: [Rule]
rules = [
      --         SOT :÷ whatever             -- GB1, taken care of in segment
      --    whatever :÷ EOT                  -- GB2, ditto
                  cr :× lf,                  -- GB3
 (control ∪ cr ∪ lf) :÷ whatever,            -- GB4
            whatever :÷ (control ∪ cr ∪ lf), -- GB5
                   l :× (l ∪ v ∪ lv ∪ lvt),  -- GB6
            (lv ∪ v) :× (v ∪ t),             -- GB7
           (lvt ∪ t) :× t,                   -- GB8
   regionalIndicator :× regionalIndicator,   -- GB8a
            whatever :× extend,              -- GB9
                 -- TODO
            whatever :÷ whatever             -- GB10
 ]


{-
segment :: [Char] -> [[Char]]
segment [] = []
segment (c:cs) = go [c] c cs
    where
      go :: [Char] -> Char -> [Char] -> [[Char]]
      go gr a []                     = [ gr ]
      go gr a (b:rest) | isBreak a b = gr : go [b] b rest
                       | otherwise   = go (gr<>[b]) b rest
-}

segment :: [CodePoint] -> [Grapheme]
segment = groupBy ((not.) . isBreak)


data Decision = Break | Don'tBreak
                deriving (Eq,Show)



isBreak :: Char -> Char -> Bool
isBreak a b | (Break:_) <- results = True
            | otherwise            = False
    where
      results = catMaybes . map (outcome a b) $ rules

outcome :: Char -> Char -> Rule -> Maybe Decision
outcome a b (x:×y) | match a b x y = Just Don'tBreak
outcome a b (x:÷y) | match a b x y = Just Break
outcome _ _ _                      = Nothing

match a b x y = CSet.member a x && CSet.member b y


play :: IO ()
play = mapM_ (\r -> print $ outcome 'a' '\x0303' r) rules
