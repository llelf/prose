module Prose.Normalization where

import qualified Prose.Properties.DecompD as D
import qualified Prose.Properties.CombiningClass as CC
import qualified Data.Map as M
import qualified Data.List.Sequences as ListSeq
import Control.Monad (ap)
import Data.List
import Data.Ord

decomposeChar :: Char -> [Char]
decomposeChar c = M.findWithDefault [c] c D.decompd

decomposeFully :: Char -> [Char]
decomposeFully = fst . head
                     . dropWhile (uncurry (/=))
                     . ap zip tail
                     . iterate (concatMap decomposeChar)
                     . (:[])



combiningClassOf :: Char -> Int
combiningClassOf c = M.findWithDefault 0 c CC.combiningclass



decompose :: [Char] -> [Char]
decompose = map fst
            . concatMap (sortBy (comparing snd))
            . ListSeq.splitSeq brk
            . map (ap (,) combiningClassOf)
            . concatMap decomposeFully
    where brk (_,cca) (_,0) | cca/=0 = False
          brk _ _                    = True



