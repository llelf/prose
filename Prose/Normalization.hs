-- |
-- Module      : Prose.Normalization
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--
module Prose.Normalization where

import qualified Prose.Properties.DecompD as NFD
import qualified Prose.Properties.DecompKD as NFKD
import qualified Prose.Properties.CombiningClass as CC
import qualified Prose.Properties.Comp as Comp
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.Sequences as ListSeq
import Control.Monad (ap)
import Control.Applicative ((<$>))
import Data.List
import Data.Ord
import Data.Tuple (swap)
import Data.Monoid

type DecompMap = M.Map Char [Char]
type CompMap = M.Map [Char] Char

newtype Decomposed = Decomposed { unDecomposed :: [Char] }

decomposeChar :: DecompMap -> Char -> [Char]
decomposeChar mp c = M.findWithDefault [c] c mp

decomposeFully :: DecompMap -> Char -> Decomposed
decomposeFully mp = Decomposed . fst . head
                     . dropWhile (uncurry (/=))
                     . ap zip tail
                     . iterate (concatMap (decomposeChar mp))
                     . (:[])

composeChars :: CompMap -> [Char] -> Maybe Char
composeChars mp cs = M.lookup cs mp

composeOneChar mp c = composeChars mp [c]
composeTwoChars mp a b = composeChars mp [a,b]


combiningClassOf :: Char -> Int
combiningClassOf c = M.findWithDefault 0 c CC.combiningclass


decomposeD, decomposeKD :: [Char] -> [Char]
decomposeD  = decompose NFD.decompd
decomposeKD = decompose NFKD.decompkd


splitToCCClusters :: [Char] -> [[(Char,Int)]]
splitToCCClusters = ListSeq.splitSeq brk
                    . map (ap (,) combiningClassOf)
    where brk _ (_,0) = False
          brk _ _     = True


decompose :: DecompMap -> [Char] -> [Char]
decompose mp = map fst
            . concatMap (sortBy (comparing snd))
            . splitToCCClusters
            . concatMap (unDecomposed . decomposeFully mp)


-- | try to compose a and b in sequence <a … pv b>
--   see Unicode7.0:3.11:D115
canCompose :: Char -> Char -> Char -> Maybe Char
canCompose a b pv | bcc==0 && pvcc==0 = composed
                  | pvcc < bcc        = composed
                  | otherwise         = Nothing
    where composed = composeTwoChars Comp.comp a b
          bcc  = combiningClassOf b
          pvcc = combiningClassOf pv


composeC :: [Char] -> [Char]
composeC = M.elems . compose0 . decomposeD


compose0 :: [Char] -> M.Map Int Char
compose0 [] = M.empty
compose0 s = go 1 2 . M.fromList . zip [1..] $ s
    where
      -- Unicode Standard, 3.11:D117
      -- Canonical Composition Algorithm

      --    current starter   current char
      go :: Int ->            Int          -> M.Map Int Char -> M.Map Int Char
      go _ i str | i > max = str
                       where (max,_) = M.findMax str

      go s i str = case canCompose sc ic pc of
                     -- we can compose <Ic, Sc> with X:
                     -- change Ic to X, remove Sc
                     Just x ->  go s (i+1) . M.adjust (const x) s . M.delete i $ str

                     -- cannot compose, but see if we have a new starter (ccc==0)
                     Nothing -> go s' (i+1) str
                         where s' | combiningClassOf ic == 0 = i
                                  | otherwise                = s
          where
            sc = str M.! s
            ic = str M.! i
            Just (_,pc) = M.lookupLT i str

{-
compose' [] = []
compose' s = map fst $ go ss (Just 1) ss'
    where (ss:ss') = map mapFromList . splitToCCClusters $ decomposeD s
          mapFromList = M.fromList . zip [1..]
          mapToList = M.elems

          go clu (Just i) rest
              = case co c p Nothing of
                  Just x  -> go (x : (cs\\[p])) i' rest
                  Nothing -> go clu             i' rest
              where Just (c,cs) = M.minView clu
                    i' = fst <$> M.lookupGT i clu
                    p = clu M.! i

          go clu Nothing rest@(ls0:ls')
              = case co c l Nothing of
                  Just x  ->        go ([x] <> cs <> ls) ls     ls'
                  Nothing -> clu <> go ls0               ls0    ls'
              where Just (c,cs) = M.minView clu

          go clu Nothing [] = mapToList clu
-}
