-- |
-- Module      :  ELynx.Distribution.Types
-- Description :  Data types for distributions on trees
-- Copyright   :  (c) Dominik Schrempf 2018
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 16 12:21:57 2018.
module ELynx.Distribution.Types
  ( Time,
    Rate,
  )
where

-- | Branch lengths are measured in time.
type Time = Double

-- | Birth or death rates.
type Rate = Double
