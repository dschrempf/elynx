{- |
Module      :  ELynx.Data.Tree.Bipartition
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

module ELynx.Data.Tree.Bipartition
  ( -- * The 'Bipartition' data type.
    Bipartition ()
  , bps
  , bp
  , bpmap
  , bphuman
    -- * Working with 'Bipartition's.
  , bipartitions
  , bipartitionToBranchLength
  ) where

import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Set                  as S
import           Data.Tree

import           ELynx.Data.Tree.Partition
import           ELynx.Data.Tree.Tree

-- | Each branch of a tree partitions the leaves of the tree into two
-- 'Partition's, or a bipartition. Also the order of the two partitions of the
-- 'Bipartition' is not important (see the 'Eq' instance).
newtype Bipartition a = Bipartition {bps :: (Partition a, Partition a) -- ^ Tuple of partitions
                                    }
  deriving (Show, Read)

-- I decided to NOT provide a human readable show instance because in this case,
-- I need the following identity to hold:
--
-- > read . show = id
--
-- This identity is met by the derived instance anyways. A more human readable
-- instance would most likely violate the identity.

-- | Show a bipartition in a human readable form. Use a provided function to
-- extract the valuable information.
bphuman :: (a -> String) -> Bipartition a -> String
bphuman f (Bipartition (x, y)) = "(" ++ pshow f x ++ "|" ++ pshow f y ++  ")"

-- | Create a bipartition from two 'S.Set's.
bp :: Ord a => Partition a -> Partition a -> Bipartition a
bp xs ys = if xs >= ys
         then Bipartition (xs, ys)
         else Bipartition (ys, xs)

-- | Map a function over all elements in the 'Bipartition's.
bpmap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpmap f (Bipartition (x, y)) = bp (pmap f x) (pmap f y)

-- | Create a bipartition from two 'S.Set's.
bpwith :: (Ord a, Ord b) => (a -> b) -> Partition a -> Partition a -> Bipartition b
bpwith f x y = bpmap f $ bp x y

instance (Eq a) => Eq (Bipartition a) where
  Bipartition x == Bipartition y = x == y

instance (Ord a) => Ord (Bipartition a) where
  Bipartition x `compare` Bipartition y = x `compare` y

-- Check if a bipartition is valid. For now, only checks if one set is empty.
valid :: Bipartition a -> Bool
valid (Bipartition (xs, ys)) = not $ pnull xs || pnull ys

-- | Get all bipartitions of the tree.
bipartitions :: Ord a => Tree a -> S.Set (Bipartition a)
bipartitions t = if S.size (S.fromList ls) == length ls
                 then S.filter valid $ bipartitionsUnsafe pempty pTree
                 else error "bipartitions: The tree contains duplicate leaves."
  where ls    = leaves t
        pTree = partitionTree t

-- | See 'bipartitions', but do not check if leaves are unique.
bipartitionsUnsafe :: Ord a => Partition a -> Tree (Partition a) -> S.Set (Bipartition a)
bipartitionsUnsafe p   (Node l [] ) = S.singleton $ bp p l
-- Degree two nodes do not induce additional bipartitions.
bipartitionsUnsafe p   (Node _ [x]) = bipartitionsUnsafe p x
-- Go through the list of children and combine each of them with the rest.
bipartitionsUnsafe p t@(Node ls xs) =
  S.unions $ S.singleton (bp p ls) : [ bipartitionsUnsafe lvs x | (lvs, x) <- zip lsOthers xs ]
  where
    lsOthers = subForestGetPartitions p t

-- | For a given rose 'Tree', remove all degree two nodes and reconnect the
-- resulting disconnected pairs of branches and sum their branch lengths. For
-- this operation, a combining binary function and a unity element is required,
-- and so we need the 'Monoid' type class constraint. Now, each branch on the
-- tree defines a unique 'Bipartition' of leaves. Convert a tree into a 'M.Map'
-- from each 'Bipartition' to the length of the branch inducing the respective
-- 'Bipartition'. The relevant information about the leaves is extracted from
-- the (leaf) nodes with a given function. Also check if leaves are unique.
bipartitionToBranchLength :: (Ord a, Ord b, Monoid c)
                    => (a -> b)      -- ^ Convert node labels to leaves (usually
                                     -- leaf names)
                    -> (a -> c)      -- ^ Get length of branch attached to node
                    -> Tree a        -- ^ Tree to dissect
                    -> M.Map (Bipartition b) c
bipartitionToBranchLength f g t = if S.size (S.fromList ls) == length ls
                 then M.filterWithKey (const . valid) $
                      bipartitionToBranchUnsafe (mempty, pempty) f lAndPTree
                 else error "bipartitionToBranch: The tree contains duplicate leaves."
  where ls        = leaves t
        bTree     = fmap g t
        pTree     = partitionTree t
        lAndPTree = fromJust $ merge bTree pTree

-- | See 'bipartitionToBranch'. When calculating the map, branches separated by
-- various degree two nodes have to be combined. Hence, not only the
-- complementary partition towards the stem, but also the node label itself have
-- to be passed along.
type Info c a = (c, Partition a)

-- | See 'bipartitionToBranch', but does not check if leaves are unique. We need
-- information about the nodes, and also about the leaves of the induced sub
-- trees. Hence, we need a somewhat complicated node label type
--
-- > (a, Partition a)
bipartitionToBranchUnsafe :: (Ord a, Ord b, Monoid c)
  => Info c a
  -> (a -> b)        -- ^ Value to compare on
  -> Tree (Info c a) -- ^ Tree to dissect
  -> M.Map (Bipartition b) c
bipartitionToBranchUnsafe (l, p) f (Node (l', p') [] ) =
  M.singleton (bpwith f p p') (l <> l')
-- The branch length has to be added for degree two nodes.
bipartitionToBranchUnsafe (l, p) f (Node (l', _ ) [x]) =
  bipartitionToBranchUnsafe (l <> l', p) f x
-- Go through the list of children and combine each of them with the rest.
bipartitionToBranchUnsafe (l, p) f t@(Node (l', p') xs) =
  M.unionsWith (<>) $
  M.singleton (bpwith f p p') (l <> l') :
  [ bipartitionToBranchUnsafe (mempty, lvs) f x | (lvs, x) <- zip lvsOthers xs ]
  where
    lvsOthers = subForestGetPartitions p (fmap snd t)
