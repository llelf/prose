module Prose.Types (CodePoint,Grapheme,Prose(..))
    where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.String
import Data.Monoid

-- | Unicode code point
type CodePoint = Char

-- | Grapheme cluster
type Grapheme = [Char]


data Prose = T T.Text             -- ^ Text's utf-16 data in NFC
           -- | U BS.ByteString       -- ^ ByteString with utf-8 in NFC

           -- ^ Well…ideally, change Text’s internal represetation to utf-8


-- TODO: NF

instance Show Prose where
    show (T txt) = "\"" <> T.unpack txt <> "\"" -- XXX only printable

instance IsString Prose where
    fromString = T . T.pack


