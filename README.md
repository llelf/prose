[![Build Status](https://travis-ci.org/llelf/prose.svg?branch=master)](https://travis-ci.org/llelf/prose)



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
[HCAR](https://wiki.haskell.org/Haskell_Communities_and_Activities_Report) entry:

Many programming languages offer non-existing or very poor support for
Unicode.  While many think that Haskell is not one of them, this is
not completely true.  The way-to-go library of Haskell’s string type,
Text, only provides codepoint-level operations.  Just as a small and
very elementary example: two “Haskell café” strings, first written
with the ‘é’ character, and the second with the ‘e’ character followed
by a combining acute accent character, are obviously have a
correspondence for many real-world situations. Yet they are entirely
different and unconnected things for Text and its operations.

And even though there is text-icu library offering proper Unicode
functions, it has a form of FFI bindings to C library (and that is
painful, especially for Windows users). More so, its API is very
low-level and incomplete.

[Prose](https://github.com/llelf/prose) is (work-in-progress) pure
Haskell implementation of Unicode strings.  Right now it’s completely
inoptimized.  Implemented parts are normalization algorithms and
segmentation by graphemes and words.

[Numerals](https://github.com/llelf/numerals) is pure Haskell
implementation of CLDR (Common Language Data Repository, Unicode’s
locale data) numerals formatting.

Further reading

http://lelf.lu/prose
https://github.com/llelf/prose
https://github.com/llelf/numerals


-------


optimizations: none

|       	                                |  Prose/𝘚    |  ICU          |
|-----------------------------------------------|------------|---------------|
|segmentation/graphemes one-lang text           | 1.60ms     | 0.47ms        |
|segmentation/graphemes chars sample            | 15.84ms    | 16.30ms       |


