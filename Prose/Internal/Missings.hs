module Prose.Internal.Missings
    (defaultIgnorableCodePoint)
    where

import Data.Char
import qualified Data.CharSet as CSet

defaultIgnorableCodePoint :: CSet.CharSet
defaultIgnorableCodePoint = CSet.fromList $ map chr $ concat
 [
                  [0x00AD],
                  [0x034F],
                  [0x061C],
                  [0x115F..0x1160],
                  [0x17B4..0x17B5],
                  [0x180B..0x180D],
                  [0x180E],      
                  [0x200B..0x200F],
                  [0x202A..0x202E],
                  [0x2060..0x2064],
                  [0x2065],      
                  [0x2066..0x206F],
                  [0x3164],      
                  [0xFE00..0xFE0F],
                  [0xFEFF],      
                  [0xFFA0],      
                  [0xFFF0..0xFFF8],
                  [0x1BCA0..0x1BCA3],
                  [0x1D173..0x1D17A],
                  [0xE0000],     
                  [0xE0001],     
                  [0xE0002..0xE001F],
                  [0xE0020..0xE007F],
                  [0xE0080..0xE00FF],
                  [0xE0100..0xE01EF],
                  [0xE01F0..0xE0FFF]
 ]


