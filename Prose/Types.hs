module Prose.Types (CodePoint,Grapheme,Prose(..))
    where

import Data.Text (Text)
import Data.ByteString (ByteString)

-- | Unicode code point
type CodePoint = Char

-- | Grapheme cluster
type Grapheme = [Char]


data Prose = T Text             -- ^ Text's utf-16 data in NFC
           | U ByteString       -- ^ ByteString with utf-8 in NFC


