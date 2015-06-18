{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Monoid
import Prose.Properties.Types
import Data.Map ((!))
import Data.Char (chr)
import Data.List (intersperse,intercalate)
import Data.List.Split
import qualified Data.Map as M
import Control.Exception
import Data.Binary as Bin
import Lens.Family2

{-
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
 ("OLower",   set otherLower . readYN),
 ("ccc",      set combiningClass . read),
 ("Dash",     set dash . readYN),
 ("Hyphen",   set hyphen . readYN),
 ("QMark",    set quotationMark . readYN),
 ("Term",     set terminalPunctuation . readYN)
 ]
-}


toProp (TagOpen _ psm) = [ (ix, CharProps{..}) | ix <- ixs ]
    where _name            = ps!"na"
          _generalCategory = read $ ps!"gc"
          ixs = case (fmap readCodePoint . (`M.lookup` ps)) <$> ["cp","first-cp","last-cp"] of
                  [Just x, Nothing, Nothing] -> [x]
                  [Nothing, Just a, Just b]  -> [a..b]
          [_nfd_qc, _nfkd_qc] = readYN . (ps!) <$> ["NFD_QC", "NFKD_QC"]
          [_nfc_qc, _nfkc_qc] = readQCValue . (ps!) <$> ["NFC_QC", "NFKC_QC"]
          [_upper, _otherUpper, _lower, _otherLower] = readYN . (ps!) <$> ["Upper","OUpper","Lower","OLower"]
          _combiningClass      = read $ ps!"ccc"
          _dash                = readYN $ ps!"Dash"
          _hyphen              = readYN $ ps!"Hyphen"
          _quotationMark       = readYN $ ps!"QMark"
          _terminalPunctuation = readYN $ ps!"Term"
          _diactric            = readYN $ ps!"Dia"
          _extender            = readYN $ ps!"Ext"
          _decomposition       = readDecomp $ ps!"dm"
          ps = M.fromList psm





readQCValue :: String -> QCValue
readQCValue "Y" = QCYes
readQCValue "N" = QCNo
readQCValue "M" = QCMaybe

readYN :: String -> Bool
readYN "Y" = True
readYN "N" = False

readCodePoint :: String -> Char
readCodePoint = chr . read . ("0x"++)

readDecomp "#" = DCSelf
readDecomp s = DC . map readCodePoint $ words s



main = do props <- (readSavedProps
                   `catch` \(e::IOException) ->
                   saveProps)
          print $ length props



readSavedProps = Bin.decode <$> L.readFile "properties.data"


saveProps = do
  input <- readFile "data/ucd.all.flat.xml"
  let parsed = parseTags input
  let props = concatMap toProp . filter isChar $ parsed :: [(Char,CharProps)]
  --writeUCD props
  writeBinary props
  pure props
      where isChar (TagOpen "char" _) = True
            isChar _                  = False



instance Binary CharProps
instance Binary GeneralCategory
instance Binary QCValue
instance Binary Decomp

writeBinary props = L.writeFile "properties.data" (Bin.encode props)

writeUCD props = do
  let chunks = chunksOf 1024 props
  writeFile file $ unlines ["module Prose.UCD (ucd) where",
                            "import Prose.Properties.Types"]

  forM_ (zip [1..] chunks) $ \(i,p) -> writePart i p

  appendFile file $ "ucd = concat["
                 <> intercalate "++" ["ucd"<>show i | i<-[1..length chunks]]
                 <> "]"

      where file = "Prose/UCD.hs"

            writePart :: Int -> [(Char,CharProps)] -> IO ()
            writePart n pp = appendFile file $ concat [
                  "ucd"<>show n<>" :: [(Char,CharProps)]\n",
                  "ucd"<>show n<>" = \n",
                  "  " <> show pp,
                  "\n"]




