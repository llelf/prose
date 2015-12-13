-- |
-- Module      : Prose
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--

module Prose (Prose) where

import Prose.Types
import qualified Prose.Segmentation.Graphemes as Gr
import qualified Data.Text as T

graphemes :: Prose -> [Prose]
graphemes (T txt) = map (T . T.pack) . Gr.segment . T.unpack $ txt



