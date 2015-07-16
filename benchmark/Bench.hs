
import Criterion.Main
import System.IO.Unsafe
import qualified Prose.Segmentation.Graphemes as Graph
import qualified Prose.Segmentation.Words as Word
import qualified Prose.Normalization as Norm
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU
import Control.DeepSeq (NFData)
import Data.Monoid
import Prose

load = unsafePerformIO . readFile . ("benchmark/"++)

str_english  = load "english-2.5k.txt"
str_japanese = load "japanese-5k.txt"
str_chars    = load "characters.txt"
str_zalgo    = load "zalgo.txt"

testData = [("En",str_english), ("Ja",str_japanese), ("Chr",str_chars), ("Z",str_zalgo)]

charBreak = ICU.breakCharacter ICU.Root
wordBreak = ICU.breakWord ICU.Root


benchBoth :: (NFData r, NFData q)
             => String -> (T.Text -> r) -> (String -> q) -> String -> [Benchmark]
benchBoth name icu prose str = [
   bench (name<>"-icu") (nf icu (T.pack str)),
   bench (name<>"-p")   (nf prose str)
 ]

benchThese :: (NFData r, NFData q)
              => (T.Text -> r) -> (String -> q) -> [Benchmark]
benchThese icu prose = concat [ benchBoth name icu prose str | (name,str) <- testData ]

main = defaultMain [
        bgroup "seg"
        [
           bgroup "graph" $ benchThese (ICU.breaks charBreak) Graph.segment,
           bgroup "words" $ benchThese (ICU.breaks wordBreak) Word.segment
          -- bgroup "wordsT" $ benchThese (ICU.breaks wordBreak) Word.segmentT
        ],

        bgroup "norm"
        [
           bgroup "nfd"  $ benchThese (ICU.normalize ICU.NFD) Norm.decomposeD,
           bgroup "nfkd" $ benchThese (ICU.normalize ICU.NFKD) Norm.decomposeKD,
           bgroup "nfc"  $ benchThese (ICU.normalize ICU.NFC) Norm.composeC
        ]

       ]
