{-# LANGUAGE ScopedTypeVariables #-}
module NormalizationTests where

import Prose.Internal.NormalizationTest as NT
import Prose.Normalization as Norm

g1_check = all ok NT.normalizationtest
    where ok ([src,nfc,nfd,nfkc,nfkd] :: [String])
              -- TR44; NormalizationTest.txt
              = and [nfd  == Norm.decomposeD src,
                     nfd  == Norm.decomposeD nfc,
                     nfd  == Norm.decomposeD nfd,
                     nfkd == Norm.decomposeD nfkc,
                     nfkd == Norm.decomposeD nfkd,

                     nfkd  == Norm.decomposeKD src,
                     nfkd  == Norm.decomposeKD nfc,
                     nfkd  == Norm.decomposeKD nfd,
                     nfkd  == Norm.decomposeKD nfkc,
                     nfkd  == Norm.decomposeKD nfkd]
                || error src

