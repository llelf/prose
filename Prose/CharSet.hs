module Prose.CharSet ((∩),(∪),(⊙),(∖),
                      module Data.CharSet)
    where

import Data.CharSet hiding (map)


(⊙) :: [Char] -> CharSet
(⊙) = fromList

(∩) = intersection
(∪) = union
infixl 7 ∩
infixl 6 ∪

(∖) = difference
infixl 6 ∖
