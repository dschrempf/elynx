{- |
Module      :  ELynx.Tools.Definitions
Description :  Some global definitions
Copyright   :  (c) Dominik Schrempf 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Jan 25 17:17:41 2019.

-}

module ELynx.Tools.Definitions
  ( -- * Definitions
    chunksize
  , eps
  , eps'
  , precision
  , version
  )
where

import Data.Version

-- XXX: Manually handled, because automatic handling with @Paths_...@ modules
-- results in errors during source tarball creation and upload.

-- Careful, this version number has to be synchronized with the one specified in
-- the *.cabal files. This is done with the @bump-version@ script.

-- | ELynx version.
version :: Version
version = makeVersion [0, 2, 1]

-- | Required precision when comparing 'Double' values.
eps :: Double
eps = 1e-12

-- | Relaxed required precision when comparing 'Double' values. Often, input
-- files do not provide the required double precision. In this case, it is only
-- possible to check input with relaxed bounds.
eps' :: Double
eps' = 1e-5

-- | Default output precision.
precision :: Int
precision = 3

-- | Default chunk size for parallel evaluations.
chunksize :: Int
chunksize = 500
