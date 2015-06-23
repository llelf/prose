
module Prose (Prose) where

import Prose.Types
import qualified Prose.Segmentation.Graphemes as Gr
import qualified Data.Text as T

graphemes :: Prose -> [Prose]
graphemes (T txt) = map (T . T.pack) . Gr.segment . T.unpack $ txt



