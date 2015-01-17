
import Criterion.Main
import System.IO.Unsafe
import Prose
import Prose.Segmentation.Graphemes as Graph
import Data.Text as T
import Data.Text.ICU as ICU

load = unsafePerformIO . readFile . ("benchmark/"++)

str_english  = load "english-2.5k.txt"
str_japanese = load "japanese-5k.txt"
str_chars    = load "characters.txt"

charBreak = ICU.breakCharacter ICU.Root


main = defaultMain [
        bgroup "seg/graph" [
         bench "P/E" (nf Graph.segment str_english),
         bench "P/J" (nf Graph.segment str_japanese),
         bench "P/C" (nf Graph.segment str_chars),
         bench "ICU/E" (nf (ICU.breaks charBreak) (T.pack str_english)),
         bench "ICU/J" (nf (ICU.breaks charBreak) (T.pack str_japanese)),
         bench "ICU/C" (nf (ICU.breaks charBreak) (T.pack str_chars))
        ]
       ]
