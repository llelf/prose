{-# LANGUAGE RecordWildCards #-}


module Main where

import Control.Monad
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Prose.Properties.Types
import Data.Map ((!))
import Data.Char (chr)
import qualified Data.Map as M
import Lens.Family2

propsMap :: [(String, String -> CharProps -> CharProps)]
propsMap = [
 ("na",       set name),
 ("gc",       set generalCategory . read),
 ("NFD_QC",   set nfd_qc . readYN),
 ("NFKD_QC",  set nfkd_qc . readYN),
 ("NFC_QC",   set nfc_qc . readQCValue),
 ("NFKC_QC",  set nfkc_qc . readQCValue),
 ("Upper",    set upper . readYN),
 ("OUpper",   set otherUpper . readYN),
 ("Lower",    set lower . readYN),
 ("OLower",   set otherLower . readYN)
 ]


toProp (TagOpen _ psm) = (chr ix, CharProps{..})
    where _name            = ps!"na"
          _generalCategory = read $ ps!"gc"
          ix = read . ("0x"++) $ ps!"cp" :: Int
          [_nfd_qc, _nfkd_qc] = readYN . (ps!) <$> ["NFD_QC", "NFKD_QC"]
          [_nfc_qc, _nfkc_qc] = readQCValue . (ps!) <$> ["NFC_QC", "NFKC_QC"]
          [_upper, _otherUpper, _lower, _otherLower] = readYN . (ps!) <$> ["Upper","OUpper","Lower","OLower"]
          ps = M.fromList psm





readQCValue :: String -> QCValue
readQCValue "Y" = QCYes
readQCValue "N" = QCNo
readQCValue "M" = QCMaybe

readYN :: String -> Bool
readYN "Y" = True
readYN "N" = False

main = do
  input <- readFile "data/ucd.all.flat.xml"
  let parsed = parseTags input
  let props = map toProp . filter isChar $ parsed
  print $ props
      where isChar (TagOpen "char" _) = True
            isChar _                  = False



