{-# LANGUAGE ViewPatterns #-}
module Prose.Normalization.D where

import qualified Prose.Properties.Decomp as D
import qualified Data.Map as M

decomposeChar :: Char -> [Char]
decomposeChar c | Just ds <- M.lookup c D.decomp = ds
                | otherwise                      = [c]

decompose :: [Char] -> [Char]
decompose = concatMap decomposeChar

