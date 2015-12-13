-- |
-- Module      : Prose.Segmentation.Common
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--
module Prose.Segmentation.Common where

import Prose.CharSet as CSet hiding (map)
import Prose.CharSet (CharSet)

cr, lf :: CharSet
cr = CSet.singleton '\x000d'
lf = CSet.singleton '\x000a'

whatever :: CharSet
whatever = CSet.full



