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
          Bipartition()
        , bps
        , bp
        , bpmap
        , bphuman
        , sshow
        -- * Working with 'Bipartition's.
        , bipartition
        , bipartitions
        , bipartitionToBranchLength
        , compatible
        )
where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Set                      as S
import           Data.Tree

import           ELynx.Data.Tree.Tree

-- | Each branch of a tree partitions the leaves of the tree into two
-- 'Subset's, or a bipartition. Also the order of the two partitions of the
-- 'Bipartition' is not important (see the 'Eq' instance).
newtype Bipartition a = Bipartition {bps :: (S.Set a, S.Set a) -- ^ Tuple of partitions
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
bphuman f (Bipartition (x, y)) = "(" ++ sshow f x ++ "|" ++ sshow f y ++ ")"

-- | Show the elements of a subset in a human readable way.
sshow :: (a -> String) -> S.Set a -> String
sshow f = intercalate "," . map f . S.toList

-- | Create a bipartition from two 'Subset's.
bp :: Ord a => S.Set a -> S.Set a -> Bipartition a
bp xs ys = if xs >= ys then Bipartition (xs, ys) else Bipartition (ys, xs)

-- | Map a function over all elements in the 'Bipartition's.
bpmap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpmap f (Bipartition (x, y)) = bp (S.map f x) (S.map f y)

-- | Create a bipartition from two 'S.Set's.
bpwith :: (Ord a, Ord b) => (a -> b) -> S.Set a -> S.Set a -> Bipartition b
bpwith f x y = bpmap f $ bp x y

instance (Eq a) => Eq (Bipartition a) where
        Bipartition x == Bipartition y = x == y

instance (Ord a) => Ord (Bipartition a) where
        Bipartition x `compare` Bipartition y = x `compare` y

-- Check if a bipartition is valid. For now, only checks if one set is empty.
valid :: Bipartition a -> Bool
valid (Bipartition (xs, ys)) = not $ S.null xs || S.null ys

-- | For a bifurcating root, get the bipartition induced by the root node.
bipartition :: Ord a => Tree a -> Bipartition a
bipartition (Node _ [x, y]) = bp (S.fromList $ leaves x) (S.fromList $ leaves y)
bipartition _               = error "Root node is not bifurcating."

-- | Get all bipartitions of the tree.
bipartitions :: Ord a => Tree a -> S.Set (Bipartition a)
bipartitions t = if S.size (S.fromList ls) == length ls
        then S.filter valid $ bipartitionsUnsafe S.empty pTree
        else error "bipartitions: The tree contains duplicate leaves."
    where
        ls    = leaves t
        pTree = partitionTree t

-- See 'bipartitions', but do not check if leaves are unique.
bipartitionsUnsafe
        :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Bipartition a)
bipartitionsUnsafe p (Node l [] ) = S.singleton $ bp p l
-- Degree two nodes do not induce additional bipartitions.
bipartitionsUnsafe p (Node _ [x]) = bipartitionsUnsafe p x
-- Go through the list of children and combine each of them with the rest.
bipartitionsUnsafe p t@(Node ls xs) =
        S.unions
                $ S.singleton (bp p ls)
                : [ bipartitionsUnsafe lvs x | (lvs, x) <- zip lsOthers xs ]
        where lsOthers = subForestGetSubsets p t

-- | For a given rose 'Tree', remove all degree two nodes and reconnect the
-- resulting disconnected pairs of branches and sum their branch lengths. Since
-- the induced bipartitions of the daughter branches of a bifurcating root node
-- are equal, the branches are also combined in this case. See
-- http://evolution.genetics.washington.edu/phylip/doc/treedist.html and how
-- unrooted trees should be handled.
--
-- For this operation, a combining binary function and a unity element is
-- required. These requirements are encoded in the 'Monoid' type class
-- constraint. Now, each branch on the tree defines a unique 'Bipartition' of
-- leaves. Convert a tree into a 'M.Map' from each 'Bipartition' to the length
-- of the branch inducing the respective 'Bipartition'. The relevant information
-- about the leaves is extracted from the (leaf) nodes with a given function.
-- Also check if leaves are unique.
bipartitionToBranchLength
        :: (Ord a, Ord b, Monoid c)
        => (a -> b)      -- ^ Convert node labels to leaves (usually
                                     -- leaf names)
        -> (a -> c)      -- ^ Get length of branch attached to node
        -> Tree a        -- ^ Tree to dissect
        -> M.Map (Bipartition b) c
bipartitionToBranchLength f g t = if S.size (S.fromList ls) == length ls
        then M.filterWithKey (const . valid) $ bipartitionToBranchLengthUnsafe
                (mempty, S.empty)
                f
                lAndPTree
        else
                error
                        "bipartitionToBranchLength: The tree contains duplicate leaves."
    where
        ls        = leaves t
        bTree     = fmap g t
        pTree     = partitionTree t
        lAndPTree = fromJust $ merge bTree pTree

-- | See 'bipartitionToBranchLength'. When calculating the map, branches
-- separated by various degree two nodes have to be combined. Hence, not only
-- the complementary partition towards the stem, but also the node label itself
-- have to be passed along.
type Info c a = (c, S.Set a)

-- | See 'bipartitionToBranchLength', but does not check if leaves are unique.
-- We need information about the nodes, and also about the leaves of the induced
-- sub trees. Hence, we need a somewhat complicated node label type
--
-- > (a, S.Set a)
bipartitionToBranchLengthUnsafe
        :: (Ord a, Ord b, Monoid c)
        => Info c a
        -> (a -> b)        -- ^ Value to compare on
        -> Tree (Info c a) -- ^ Tree to dissect
        -> M.Map (Bipartition b) c
bipartitionToBranchLengthUnsafe (l, p) f (Node (l', p') []) =
        M.singleton (bpwith f p p') (l <> l')
-- The branch length has to be added for degree two nodes.
bipartitionToBranchLengthUnsafe (l, p) f (Node (l', _) [x]) =
        bipartitionToBranchLengthUnsafe (l <> l', p) f x
-- Go through the list of children and combine each of them with the rest.
bipartitionToBranchLengthUnsafe (l, p) f t@(Node (l', p') xs) =
        M.unionsWith (<>)
                $ M.singleton (bpwith f p p') (l <> l')
                : [ bipartitionToBranchLengthUnsafe (mempty, lvs) f x
                  | (lvs, x) <- zip lvsOthers xs
                  ]
        where lvsOthers = subForestGetSubsets p (fmap snd t)

-- | Determine compatibility between an bipartition and a subset. If both
-- subsets of the bipartition share elements with the given subset, the
-- bipartition is incompatible with this subset. If all elements of the subset
-- are either not in the bipartition or mapping to one of the two subsets of the
-- bipartition, the bipartition and the subset are compatible. See also
-- 'ELynx.Data.Tree.Multipartition.compatible'.
compatible :: (Show a, Ord a) => Bipartition a -> S.Set a -> Bool
-- compatible (Bipartition (l, r)) ss = sintersection l ss `sdisjoint` sintersection r ss
compatible (Bipartition (l, r)) ss = S.null lOverlap || S.null rOverlap
    where
        lOverlap = S.intersection l ss
        rOverlap = S.intersection r ss
