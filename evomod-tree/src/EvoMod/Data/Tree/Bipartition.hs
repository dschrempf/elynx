{- |
Module      :  EvoMod.Data.Tree.Bipartition
Description :  Bipartitions on trees
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Aug 30 15:28:17 2019.

'Bipartition's are weird in that
> Bipartition x y == Bipartition y x
is True.

Also,
> Bipartition x y > Bipartition y x
is False, even when @x > y@.

That's why we have to make sure that for
> Bipartition x y
we always have @x >= y@.

-}

module EvoMod.Data.Tree.Bipartition
  ( Bipartition ()
  , bp
  , bpmap
  ) where

import           Data.Set
import           Prelude  hiding (map)

-- | Bipartitions with 'Set.Set's, since order of elements within the leaf sets
-- is not important. Also the order of the two leaf sets of the bipartition is
-- not important (see 'Eq' instance definition).
newtype Bipartition a = Bipartition (Set a, Set a)
  deriving (Show)

-- | Create a bipartition from two 'Set's.
bp :: Ord a => Set a -> Set a -> Bipartition a
bp x y = if x >= y
         then Bipartition (x, y)
         else Bipartition (y, x)

instance (Eq a) => Eq (Bipartition a) where
  Bipartition x == Bipartition y = x == y

instance (Ord a) => Ord (Bipartition a) where
  Bipartition x `compare` Bipartition y = x `compare` y

-- | Map a function over all elements in the 'Bipartition's.
bpmap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpmap f (Bipartition (x, y)) = Bipartition (map f x, map f y)
