-- |
-- Module      : Prose.Types
-- Copyright   : (c) 2014–2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
-- Stability   : experimental
--
--
module Prose.Types (CodePoint,Grapheme,Prose(..))
    where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Prose.Normalization.Text as Norm
import Data.String
import Data.Monoid
import Data.Function

-- | Unicode code point
type CodePoint = Char

-- | Grapheme cluster
type Grapheme = Prose


data Prose = T { unT :: T.Text } -- ^ Text's utf-16 data
           -- | U BS.ByteString       -- ^ ByteString with utf-8

           -- ^ Well…ideally, change Text’s internal represetation to utf-8



instance Eq Prose where
    (==) = (==) `on` Norm.decomposeD . unT

instance Show Prose where
    show (T txt) = "\"" <> T.unpack txt <> "\"" -- XXX only printable

instance IsString Prose where
    fromString = T . T.pack


