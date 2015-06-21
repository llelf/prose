Pure-Haskell proper unicode strings

✓⃞ grapheme segmentation

normalization:  
  ✓⃞ NFD

-------


optimizations: none

|       	                                |  Prose/𝘚    |  ICU          |
|-----------------------------------------------|------------|---------------|
|segmentation/graphemes one-lang text           | 1.60ms     | 0.47ms        |
|segmentation/graphemes chars sample            | 15.84ms    | 16.30ms       |


