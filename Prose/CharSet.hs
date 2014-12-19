module Prose.CharSet ((∩),(∪),(⊙),
                      module Data.CharSet)
    where

import Data.CharSet


(⊙) :: [Char] -> CharSet
(⊙) = fromList

(∩) = intersection
(∪) = union
infixl 7 ∩
infixl 6 ∪


