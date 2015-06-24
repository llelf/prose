module Prose.Normalization where

import qualified Prose.Properties.DecompD as NFD
import qualified Prose.Properties.DecompKD as NFKD
import qualified Prose.Properties.CombiningClass as CC
import qualified Data.Map as M
import qualified Data.List.Sequences as ListSeq
import Control.Monad (ap)
import Data.List
import Data.Ord

type DecompMap = M.Map Char [Char]
newtype Decomposed = Decomposed { unDecomposed :: [Char] }

decomposeChar :: DecompMap -> Char -> [Char]
decomposeChar mp c = M.findWithDefault [c] c mp


decomposeFully :: DecompMap -> Char -> Decomposed
decomposeFully mp = Decomposed . fst . head
                     . dropWhile (uncurry (/=))
                     . ap zip tail
                     . iterate (concatMap (decomposeChar mp))
                     . (:[])



combiningClassOf :: Char -> Int
combiningClassOf c = M.findWithDefault 0 c CC.combiningclass


decomposeD, decomposeKD :: [Char] -> [Char]
decomposeD  = decompose NFD.decompd
decomposeKD = decompose NFKD.decompkd


decompose :: DecompMap -> [Char] -> [Char]
decompose mp = map fst
            . concatMap (sortBy (comparing snd))
            . ListSeq.splitSeq brk
            . map (ap (,) combiningClassOf)
            . concatMap (unDecomposed . decomposeFully mp)
    where brk (_,cca) (_,0) | cca/=0 = False
          brk _ _                    = True



