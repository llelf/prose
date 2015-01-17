module Prose.Internal (codepoints,
                       Text) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Prose.Types

codepoints :: Prose -> [CodePoint]
codepoints (T t) = Text.unpack t
codepoints (U bs) = UTF8.toString bs

