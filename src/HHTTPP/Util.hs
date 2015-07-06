module HHTTPP.Util where

import Data.Maybe (listToMaybe)

maybe_read :: (Read a) => String -> Maybe a
maybe_read = fmap fst . listToMaybe . reads
