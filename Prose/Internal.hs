module Prose.Internal (codepoints,
                       Text) where

import Data.Text (Text)
import qualified Data.Text as Text
import Prose.Types

codepoints :: Text -> [CodePoint]
codepoints = Text.unpack

