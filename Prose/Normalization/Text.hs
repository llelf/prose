module Prose.Normalization.Text where

import Prose.Normalization as N
import Data.Text

decomposeD :: Text -> Text
decomposeD = pack . N.decomposeD . unpack

decomposeKD :: Text -> Text
decomposeKD = pack . N.decomposeKD . unpack

composeC :: Text -> Text
composeC = pack . N.composeC . unpack




