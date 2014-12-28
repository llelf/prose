module SegmentationTests where

import Test.QuickCheck
import Data.Char
import Prose.Types
import Prose.Segmentation.Graphemes as Graph

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

