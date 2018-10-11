{- |
Module      :  StateSpace
Description :  State spaces are a collection of states.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Oct 11 07:14:57 2018.

-}


module Evol.Data.StateSpace
  ( StateSpace (..)
  ) where

import           Data.Vector.Unboxed as V
import           Data.Word

-- | State spaces collect a set of states. , 'Word16' has a range of 2^16-1
-- which should be more than enough.
--
-- A 'V.Vector' is used because access needs to be fast and because it is also
-- used by HMatrix's 'Numeric.Linear.Data'.
newtype StateSpace = I { fromI :: V.Vector Word16 }
