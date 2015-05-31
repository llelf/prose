{-# LANGUAGE TemplateHaskell #-}

module Prose.Properties.Types where

import Lens.Family.TH

data CharProps = CharProps {
      _name :: String,
      _generalCategory :: GeneralCategory,
      _upper :: Bool, _lower :: Bool,
      _otherUpper :: Bool, _otherLower :: Bool,
      _nfc_qc :: QCValue,
      _nfd_qc :: Bool,
      _nfkc_qc :: QCValue,
      _nfkd_qc :: Bool
} deriving Show


data GeneralCategory =
    Lu|Ll|Lt|             --LC
    Lm|Lo|                --L
    Mn|Mc|Me|             --M
    Nd|Nl|No|             --N
    Pc|Pd|Ps|Pe|Pi|Pf|Po| --P
    Sm|Sc|Sk|So|          --S
    Zs|Zl|Zp|             --Z
    Cc|Cf|Cs|Co|Cn        --C
        deriving (Show,Read)

data QCValue = QCYes | QCNo | QCMaybe deriving Show

makeLenses ''CharProps
