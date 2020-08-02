-- |
-- Module      :  ELynx.Tools.Definitions
-- Description :  Some global definitions
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 25 17:17:41 2019.
module ELynx.Tools.Definitions
  ( -- * Definitions
    eps,
    precision,
    version,
  )
where

import Data.Version

-- TODO: Move back to automatic version handling.

-- XXX: Manually handled, because automatic handling with @Paths_...@ modules
-- results in errors during source tarball creation and upload.

-- | ELynx version.
version :: Version
version = makeVersion [0, 3, 0]

-- | Required precision when comparing 'Double' values.
eps :: Double
eps = 1e-12

-- | Default output precision.
precision :: Int
precision = 3
