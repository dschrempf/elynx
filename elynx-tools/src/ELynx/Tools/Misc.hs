-- |
-- Module      :  ELynx.Tools.Misc
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Feb 14 13:32:19 2019.
--
-- Miscellaneous tools that do not have their own category (yet).
module ELynx.Tools.Misc
  ( -- * Weird stuff
    compose,
    allValues,
  )
where

-- | Chain a list of functions together. See https://wiki.haskell.org/Compose.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

-- | Get all values of a bounded enumerated type.
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]
