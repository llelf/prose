{-# LANGUAGE TypeOperators #-}
module Prose.Segmentation.Graphemes where

import Prose.Internal.Missings

import qualified Data.CharSet.Unicode as Unicode
import Data.Char
import Prose.CharSet as CSet hiding (map)
import Prose.CharSet (CharSet)
import Data.Monoid
import Data.List (groupBy)
import Data.Maybe

-- Unicode 7.0.0 Annex #29

cr = CSet.singleton '\x000d'
lf = CSet.singleton '\x000a'

control = Unicode.lineSeparator
            ∪ Unicode.paragraphSeparator
            ∪ Unicode.control
            ∪ Unicode.notAssigned ∩ defaultIgnorableCodePoint
            ∪ Unicode.surrogate
            ∪ Unicode.format ∩ CSet.fromList ['\xd', '\xa', '\x200c', '\x200d']

extend = CSet.fromList [ '\x0300'..'\x036f' ] -- XXX WRONG

regionalIndicator = CSet.fromList [ '\x1F1E6'..'\x1F1FF' ]

prepend = CSet.empty

spacingMark = ()
    where exceptions = [ ]

l = CSet.empty
v = CSet.empty
t = CSet.empty
lv = CSet.empty
lvt = CSet.empty

whatever = CSet.full


data Rule = CharSet :× CharSet  -- do break
          | CharSet :÷ CharSet  -- do not break

rules :: [Rule]
rules = [
      -- taken care of in segment            -- GB1
      -- ditto                               -- GB2
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

segment :: [Char] -> [[Char]]
segment = groupBy ((not.) . isBreak)


data Decision = Break | Don'tBreak | Dunno
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
