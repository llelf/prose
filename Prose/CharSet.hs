module Prose.CharSet ((∩),(∪),
                      module Data.CharSet)
    where

import Data.CharSet

(∩) = intersection
(∪) = union
infixl 7 ∩
infixl 6 ∪


