-- |
-- Module      : Prose.Properties.Types
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}


module Prose.Properties.Types where

import Lens.Micro
import Lens.Micro.TH
import GHC.Generics

data CharProps = CharProps {
      _name :: String,
      _generalCategory :: GeneralCategory,
      _upper :: Bool, _lower :: Bool,
      _otherUpper :: Bool, _otherLower :: Bool,
      _nfc_qc :: QCValue,
      _nfd_qc :: Bool,
      _nfkc_qc :: QCValue,
      _nfkd_qc :: Bool,
      _combiningClass :: Int,
      _dash :: Bool,
      _hyphen :: Bool,
      _quotationMark :: Bool,
      _terminalPunctuation :: Bool,
      _diactric :: Bool,
      _extender :: Bool,
      _decomposition :: Decomp,
      _decompositionType :: Maybe DecompType,
      _fullDecompositionExclusion :: Bool
} deriving (Show,Generic)



data GeneralCategory =
    Lu|Ll|Lt|             --LC
    Lm|Lo|                --L
    Mn|Mc|Me|             --M
    Nd|Nl|No|             --N
    Pc|Pd|Ps|Pe|Pi|Pf|Po| --P
    Sm|Sc|Sk|So|          --S
    Zs|Zl|Zp|             --Z
    Cc|Cf|Cs|Co|Cn        --C
        deriving (Show,Read,Generic)

data QCValue = QCYes | QCNo | QCMaybe deriving (Show,Generic)

data Decomp = DCSelf | DC [Char] deriving (Show,Eq,Generic)

decompositionOf :: Char -> Decomp -> [Char]
decompositionOf c DCSelf  = [c]
decompositionOf c (DC ds) = ds


data DecompType = DTCanonical | DTCompat | DTFont
                | DTNoBreak | DTInitial | DTMedial | DTFinal
                | DTIsolated | DTCircle | DTSuper | DTSub
                | DTVertical | DTWide | DTNarrow
                | DTSmall | DTSquare | DTFraction
                  deriving (Show,Eq,Generic)


makeLenses ''CharProps
