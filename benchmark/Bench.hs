
import Criterion.Main
import System.IO.Unsafe
import Prose
import Prose.Segmentation.Graphemes as Graph
import Data.Text as T
import Data.Text.ICU as ICU

str_english = unsafePerformIO $ readFile "benchmark/english-2.5k.txt"
str_japanese = unsafePerformIO $ readFile "benchmark/japanese-5k.txt"

charBreak = ICU.breakCharacter ICU.Root


main = defaultMain [
        bgroup "seg/graph" [
         bench "P/E" (nf Graph.segment str_english),
         bench "P/J" (nf Graph.segment str_japanese),
         bench "ICU/E" (nf (ICU.breaks charBreak) (T.pack str_english)),
         bench "ICU/J" (nf (ICU.breaks charBreak) (T.pack str_japanese))
        ]
       ]
