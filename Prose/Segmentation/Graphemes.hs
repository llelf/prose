-- | Grapheme Cluster Boundaries
--
-- Module      : Prose.Segmentation.Graphemes
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--

{-# LANGUAGE TypeOperators #-}
module Prose.Segmentation.Graphemes
    (segment) where

import qualified Data.CharSet.Unicode as Unicode
import Data.Char
import Prose.CharSet as CSet hiding (map)
import Prose.CharSet (CharSet)
import Data.Monoid
import Data.List (groupBy)
import Data.List.Sequences as ListSeq
import Data.Maybe

import qualified Prose.Internal.Missings as Missings
import Prose.Types
import Prose.Segmentation.Common


-- Unicode 7.0.0 Annex #29

control :: CharSet
control = Unicode.lineSeparator
            ∪ Unicode.paragraphSeparator
            ∪ Unicode.control
            ∪ Unicode.notAssigned ∩ Missings.defaultIgnorableCodePoint
            ∪ Unicode.surrogate
            ∪ (Unicode.format ∖ (⊙)['\xd', '\xa', '\x200c', '\x200d'])

extend :: CharSet
extend = Missings.graphemeExtend

regionalIndicator :: CharSet
regionalIndicator = (⊙)[ '\x1F1E6'..'\x1F1FF' ]

prepend :: CharSet
prepend = CSet.empty

spacingMark :: CharSet
spacingMark = Missings.spacingMark


l,v,t,lv,lvt :: CharSet
-- Hangul_Syllable_Type of
l = (⊙)['\x1100'..'\x115F'] ∪ (⊙)['\xA960'..'\xA97C'] -- Leading_Jamo
v = (⊙)['\x1160'..'\x11A7'] ∪ (⊙)['\xD7B0'..'\xD7C6'] -- Vowel_Jamo
t = (⊙)['\x11A8'..'\x11FF'] ∪ (⊙)['\xD7CB'..'\xD7FB'] -- Trailing_Jamo

lv = Missings.hangul_lv
lvt = Missings.hangul_lvt


-- This won't suffice for word/sentence segmentation
data Rule = CharSet :× CharSet  -- do not break
          | CharSet :÷ CharSet  -- do break



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
            whatever :× spacingMark,         -- GB9a
             prepend :× whatever,            -- GB9b
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

segment :: [CodePoint] -> [[Char]]
segment = ListSeq.splitSeq ((not.) . isBreak)


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
