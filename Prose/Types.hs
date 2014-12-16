module Prose.Types (CodePoint,Grapheme,Prose())
    where

import Data.Text (Text)

-- | Unicode code point
type CodePoint = Char

-- | Grapheme cluster
type Grapheme = [Char]


data Prose = Prose {
      nfc :: Text                 -- ^ Text's utf-16 data in NFC
}



