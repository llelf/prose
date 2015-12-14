Pure-Haskell proper unicode strings

```
λ> graphemes "བོད་ཀྱི་སྐད་ཡིག།"
["བོ","ད","་","ཀྱི","་","སྐ","ད","་","ཡི","ག","།"]
```

See [prose-lens](https://github.com/llelf/prose-lens) for a lens
interface.


segmentation:  
  ✓⃞ grapheme  
  ✓⃞ words  ⃞ tailored  
   ⃞ sentences  ⃞ tailored  
   ⃞ line-breaking  ⃞ tailored

normalization:  
  ✓⃞ NFD ✓⃞ NFKD ✓⃞ NFC  ⃞ NFKC

collating  ⃞ …  
transformation  ⃞ …  
character properties  ⃞ …  
other cldr  ⃞ …  



-------


optimizations: none

|       	                                |  Prose/𝘚    |  ICU          |
|-----------------------------------------------|------------|---------------|
|segmentation/graphemes one-lang text           | 1.60ms     | 0.47ms        |
|segmentation/graphemes chars sample            | 15.84ms    | 16.30ms       |


