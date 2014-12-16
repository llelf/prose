module Prose.Segmentation.Graphemes where

import Prose.Internal.Missings

import qualified Data.CharSet.Unicode as Unicode
import Data.Char
import Prose.CharSet as CSet


-- Unicode 7.0.0 Annex #29

cr = CSet.singleton '\x000d'
lf = CSet.singleton '\x000a'

control = Unicode.lineSeparator
            ∪ Unicode.paragraphSeparator
            ∪ Unicode.control
            ∪ Unicode.notAssigned ∩ defaultIgnorableCodePoint
            ∪ Unicode.surrogate
            ∪ Unicode.format ∩ CSet.fromList ['\xd', '\xa', '\x200c', '\x200d']

extend = ()
regionalIndicator = CSet.fromList [ '\x1F1E6'..'\x1F1FF' ]
spacingMark = ()
    where exceptions = [ ]


