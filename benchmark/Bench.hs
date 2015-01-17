
import Criterion.Main
import System.IO.Unsafe
import Prose
import Prose.Segmentation.Graphemes as Graph
import Data.Text as T
import Data.Text.ICU as ICU

str_english = unsafePerformIO $ readFile "benchmark/english.txt"

charBreak = ICU.breakCharacter ICU.Root


main = defaultMain [
        bgroup "seg/graph" [
         bench "A" (nf Graph.segment str_english),
         bench "T" (nf (ICU.breaks charBreak) (T.pack str_english))
        ]
       ]
