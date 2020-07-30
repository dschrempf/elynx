-- |
-- Module      :  ELynx.Tools.Parallel
-- Description :  Utility functions for parallel calculations
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Apr 21 15:19:44 2020.
module ELynx.Tools.Parallel
  ( parMapChunk,
  )
where

import Control.Parallel.Strategies

-- | Parallel map with given chunk size.
parMapChunk :: Int -> (a -> b) -> [a] -> [b]
parMapChunk n f as = map f as `using` parListChunk n rseq
