{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Monoid
import Control.Arrow
import Prose.Properties.Types
import Data.Map ((!))
import Data.Char (chr,toLower)
import Data.List (intersperse,intercalate)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception
import Data.Binary as Bin
import Data.Tuple (swap)
import Lens.Micro

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

type PropertiesDB = [(Char,CharProps)]

toProp :: Tag String -> PropertiesDB
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
          _decompositionType   = readDecompType $ ps!"dt"
          _fullDecompositionExclusion = readYN $ ps!"Comp_Ex"
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

readDecompType "none" = Nothing
readDecompType s = Just (table!s)
    where table = M.fromList [
                    ("can",DTCanonical),("com",DTCompat),("enc",DTCircle),
                    ("fin",DTFinal),("font",DTFont),("fra",DTFraction),
                    ("init",DTInitial),("iso",DTIsolated),("med",DTMedial),
                    ("nar",DTNarrow),("nb",DTNoBreak),("sml",DTSmall),
                    ("sqr",DTSquare),("sub",DTSub),("sup",DTSuper),
                    ("vert",DTVertical),("wide",DTWide)
              ]


genDecompositions props = do
  gen "DecompD" "Map Char [Char]" . show . decompositionsD $ props
  gen "DecompKD" "Map Char [Char]" . show . decompositionsKD $ props

decompositionsD, decompositionsKD :: PropertiesDB -> M.Map Char [Char]
decompositionsD = filteredMap ((== Just DTCanonical) . _decompositionType)
decompositionsKD = filteredMap (const True)

filteredMap :: (CharProps -> Bool) -> PropertiesDB -> M.Map Char [Char]
filteredMap filt = M.fromList
                . map (\(c,pro) -> (c, decompositionOf c (_decomposition pro)))
                . filter (\(_,pro) -> filt pro
                                      && _decomposition pro /= DCSelf)



genCompositions props = gen "Comp" "Map [Char] Char" (show dat)
    where dat = decomposeToComposeMap (fullDecExclData props) (decompositionsD props)
          dat :: M.Map [Char] Char

decomposeToComposeMap ex = M.fromList
                           . map swap
                           . filter (\(k,_) -> S.notMember k ex)
                           . filter (\(k,v) -> length v == 1 || length v == 2)
                           . M.toList


genCCC = gen "CombiningClass" "Map Char Int" . show . dat
    where dat :: PropertiesDB -> M.Map Char Int
          dat = M.fromList
                . filter (\(_,cc) -> cc /= 0)
                . map (second _combiningClass)



genFullDecExcl = gen "FullDecompExclusions" "Set Char" . show . fullDecExclData
fullDecExclData = S.fromList
                . map fst
                . filter (_fullDecompositionExclusion . snd)


gen :: String -> String -> String -> IO ()
gen name typ dat = writeFile ("Prose/Properties/" <> name <> ".hs") $
                   unlines [
                        "-- autogenerated by gather-props",
                        "module Prose.Properties."<>name<>" where",
                        "import Data." <> head (words typ),
                        "import Prose.Properties.Types",
                        name' <> " :: " <> typ,
                        name' <> "=" <> dat
                   ]
    where name' = map toLower name

main = do props <- (readSavedProps
                   `catch` \(e::IOException) ->
                   saveProps)
          print $ length props
          genDecompositions props
          genCCC props
          genFullDecExcl props
          genCompositions props



readSavedProps = Bin.decode <$> L.readFile "properties.data"


saveProps = do
  input <- readFile "data/ucd.all.flat.xml"
  let parsed = parseTags input
  let props = concatMap toProp . filter isChar $ parsed :: [(Char,CharProps)]
  writeUCD props
  writeBinary props
  pure props
      where isChar (TagOpen "char" _) = True
            isChar _                  = False



instance Binary CharProps
instance Binary GeneralCategory
instance Binary QCValue
instance Binary Decomp
instance Binary DecompType

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




