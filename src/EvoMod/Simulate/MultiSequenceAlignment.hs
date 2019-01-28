{- |
   Description :  Functions to work with transition probability matrices on rooted trees
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPLv3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Calculate transition probability matrices, map rate matrices on trees, populate
a tree with states according to a stationary distribution, etc.

The implementation of the Markov process is more than basic and can be improved in a lot of ways.

TODO: Create executable from this file.

* Changelog

-}

module EvoMod.Simulate.MultiSequenceAlignment
  (simulateMSA)
  where

import           Control.Monad.Primitive
import           Data.Tree

import qualified Data.ByteString.Lazy.Char8                  as B

import           EvoMod.Data.RateMatrix.RateMatrix
import           EvoMod.Data.Sequence.MultiSequenceAlignment
import           EvoMod.Data.Sequence.Sequence
import           EvoMod.Data.Tree.MeasurableTree
import           EvoMod.Data.Tree.NamedTree
import           EvoMod.Data.Tree.Tree
import           EvoMod.Simulate.MarkovProcessAlongTree
import           System.Random.MWC

-- | Simulate a 'MultiSequenceAlignment' for a given substitution model and
-- phylogenetic tree.
simulateMSA :: (PrimMonad m, Measurable a, Named a)
            => Int -> RateMatrix -> Tree a -> Gen (PrimState m)
            -> m MultiSequenceAlignment
simulateMSA n q t g = do
  statesTree <- simulateNSitesAlongTree n q t g
  let leafNames = map name $ leafs t
      leafStates = leafs statesTree
      sequences = [ toSequence i (B.pack $ map toEnum ss) | (i, ss) <- zip leafNames leafStates ]
  return $ fromSequenceList sequences
