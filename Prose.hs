-- |
-- Module      : Prose
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--

module Prose (Prose,
              graphemes, words,
              normC, normD, normKD) where

import Prelude hiding (words)
import Prose.Types
import qualified Prose.Segmentation.Graphemes as Gr
import qualified Prose.Segmentation.Words as Wd
import qualified Prose.Normalization.Text as Nm
import qualified Data.Text as T

graphemes :: Prose -> [Prose]
graphemes (T txt) = map (T . T.pack) . Gr.segment . T.unpack $ txt

words :: Prose -> [Prose]
words (T txt) = map T . Wd.segmentT $ txt

normC :: Prose -> Prose
normC (T txt) = T . Nm.composeC $ txt

normD :: Prose -> Prose
normD (T txt) = T . Nm.decomposeD $ txt

normKD :: Prose -> Prose
normKD (T txt) = T . Nm.decomposeKD $ txt


