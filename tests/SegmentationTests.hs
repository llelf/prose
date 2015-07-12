module SegmentationTests where

import Test.QuickCheck
import Data.Char
import Prose.Types
import Prose.Segmentation.Graphemes as Graph
import Prose.Segmentation.Words as Word
import Prose.Internal.GraphemeBreakTest as GBT
import Prose.Internal.WordBreakTest as WBT

a1s = "óòo̧ö" :: String
a1accents = filter (not.isLetter) a1s

newtype G1 = G1 String deriving Show

instance Arbitrary G1 where
    arbitrary = do alen <- choose (0,1) :: Gen Int
                   acc <- vectorOf alen (elements a1accents)
                   c <- elements ['a'..'z']
                   return $ G1 $ [c] ++ acc

size :: [Grapheme] -> Int
size = sum . map length

g1_prop s = size (Graph.segment s) == length s


properlyBroken segm broken = segm (concat broken) == broken
                        || error (show broken)

-- GraphemeBreakTest
g2_check = all (properlyBroken Graph.segment) GBT.graphemebreaktest

-- WordBreakTest
w1_check = all (properlyBroken Word.segment) WBT.wordbreaktest



