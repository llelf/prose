
module Prose.Properties.Types where

data CharProps = CharProps {
      name :: String,
      generalCategory :: GeneralCategory,
      upper :: Bool, lower :: Bool,
      otherUpper :: Bool, otherLower :: Bool,
      nfc_qc :: QCValue,
      nfd_qc :: Bool,
      nfkc_qc :: QCValue,
      nfkd_qc :: Bool
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

