-- |
-- Module      :  ELynx.Data.Tree.Bipartition
-- Description :  Bipartitions on trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 30 15:28:17 2019.
--
-- 'Bipartition's are weird in that
-- > Bipartition x y == Bipartition y x
-- is True.
--
-- Also,
-- > Bipartition x y > Bipartition y x
-- is False, even when @x > y@.
--
-- That's why we have to make sure that for
-- > Bipartition x y
-- we always have @x >= y@.
module ELynx.Data.Tree.Bipartition
  ( -- * The 'Bipartition' data type
    Bipartition (fromBipartition),
    bp,
    toSet,
    bpHuman,
    bpMap,

    -- * Working with 'Bipartition's
    bipartition,
    bipartitions,
    -- bipartitionToBranchLength,
    -- bpcompatible,
  )
where

import Data.List
-- import qualified Data.Map as M
-- import Data.Map (Map)
-- import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Data.Tree.Tree
import ELynx.Data.Tree.Phylogeny

-- | Each branch of a tree partitions the leaves of the tree into two subsets,
-- or a bipartition.
--
-- The order of the two subsets of a 'Bipartition' is meaningless. We ensure by
-- construction that the smaller subset comes first, and so, equality checks are
-- meaningful.
newtype Bipartition a = Bipartition
  { fromBipartition :: (Set a, Set a)
  }
  deriving (Eq, Ord, Show, Read)

-- | Create a bipartition from two sets.
--
-- Ensure that the smaller set comes first.
bp :: Ord a => Set a -> Set a -> Bipartition a
bp xs ys = if xs >= ys then Bipartition (xs, ys) else Bipartition (ys, xs)

-- | Conversion to a set containing both partitions.
toSet :: Ord a => Bipartition a -> Set a
toSet (Bipartition (x, y)) = S.union x y

-- I decided not to provide a human readable show instance because I need the
-- following identity to hold:
--
-- > read . show = id
--
-- This identity is met by the derived instance anyways. A more human readable
-- instance would most likely violate the identity.

-- | Show a bipartition in a human readable format. Use a provided function to
-- extract information of interest.
bpHuman :: Show a => Bipartition a -> String
bpHuman (Bipartition (x, y)) = "(" ++ setShow x ++ "|" ++ setShow y ++ ")"

-- Show the elements of a set in a human readable format.
setShow :: Show a => Set a -> String
setShow = intercalate "," . map show . S.toList

-- | Map a function over all elements in the 'Bipartition'.
bpMap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpMap f (Bipartition (x, y)) = bp (S.map f x) (S.map f y)

-- Check if a bipartition is valid. For now, only checks that no set is empty.
bpValid :: Bipartition a -> Bool
bpValid (Bipartition (xs, ys)) = not $ S.null xs || S.null ys

-- | For a bifurcating root, get the bipartition induced by the root node.
bipartition :: Ord a => Tree e a -> Bipartition a
bipartition (Node _ _ [x, y]) =
  bp (S.fromList $ leaves x) (S.fromList $ leaves y)
bipartition _ = error "Root node is not bifurcating."

-- | Get all bipartitions of the tree.
bipartitions :: Ord a => Tree e a -> Maybe (Set (Bipartition a))
bipartitions t
  | valid t =
    Just $ S.filter bpValid $ bipartitionsUnsafe S.empty $ S.fromList <$> partitionTree t
  | otherwise = Nothing

-- Report the complementary leaves for each child.
getComplementaryLeaves ::
  (Ord a) =>
  -- Complementary leaves.
  Set a ->
  -- Tree with node labels storing leaves.
  Tree e (Set a) ->
  [Set a]
getComplementaryLeaves p (Node _ _ ts) =
  [ S.unions $ p : take i lvsChildren ++ drop (i + 1) lvsChildren
    | i <- [0 .. (n -1)]
  ]
  where
    n = length ts
    lvsChildren = map label ts

-- See 'bipartitions', but do not check if leaves are unique, nor if
-- bipartitions are valid.
bipartitionsUnsafe :: Ord a => Set a -> Tree e (Set a) -> Set (Bipartition a)
bipartitionsUnsafe p (Node _ l []) = S.singleton $ bp p l
-- Degree two nodes do not induce additional bipartitions.
bipartitionsUnsafe p (Node _ _ [t]) = bipartitionsUnsafe p t
-- Go through the list of children and combine each of them with the rest.
bipartitionsUnsafe p t@(Node _ l ts) =
  S.unions $
    S.singleton (bp p l) :
      [bipartitionsUnsafe c s | (c, s) <- zip complementaryLeaves ts]
  where
    complementaryLeaves = getComplementaryLeaves p t

-- TODO: Continue here.

-- -- | For a given rose 'Tree', remove all degree two nodes and reconnect the
-- -- resulting disconnected pairs of branches and sum their branch lengths.
-- --
-- -- Since the induced bipartitions of the daughter branches of a bifurcating root
-- -- node are equal, the branches are also combined in this case. See
-- -- http://evolution.genetics.washington.edu/phylip/doc/treedist.html and how
-- -- unrooted trees should be handled.
-- --
-- -- For this operation, a combining binary function and a unity element is
-- -- required. These requirements are encoded in the 'Monoid' type class
-- -- constraint. Now, each branch on the tree defines a unique 'Bipartition' of
-- -- leaves. Convert a tree into a 'Map' from each 'Bipartition' to the length
-- -- of the branch inducing the respective 'Bipartition'. The relevant information
-- -- about the leaves is extracted from the (leaf) nodes with a given function.
-- -- Also check if leaves are unique.
-- bipartitionToBranchLength ::
--   (Ord a, Ord b, Monoid c) =>
--   -- | Convert node labels to leaves (usually
--   -- leaf names)
--   (a -> b) ->
--   -- | Get length of branch attached to node
--   (a -> c) ->
--   -- | Tree to dissect
--   Tree e a ->
--   Map (Bipartition b) c
-- bipartitionToBranchLength f g t =
--   if S.size (S.fromList ls) == length ls
--     then
--       M.filterWithKey (const . valid) $
--         bipartitionToBranchLengthUnsafe (mempty, S.empty) f lAndPTree
--     else error "bipartitionToBranchLength: The tree contains duplicate leaves."
--   where
--     ls = leaves t
--     bTree = fmap g t
--     pTree = partitionTree t
--     lAndPTree = fromJust $ merge bTree pTree

-- -- | See 'bipartitionToBranchLength'.
-- --
-- -- When calculating the map, branches separated by various degree two nodes have
-- -- to be combined. Hence, not only the complementary partition towards the stem,
-- -- but also the node label itself have to be passed along.
-- type Info c a = (c, Set a)

-- -- | See 'bipartitionToBranchLength', but does not check if leaves are unique.
-- --
-- -- We need information about the nodes, and also about the leaves of the induced
-- -- sub trees. Hence, we need a somewhat complicated node label type
-- --
-- -- > (a, Set a)
-- bipartitionToBranchLengthUnsafe ::
--   (Ord a, Ord b, Monoid c) =>
--   Info c a ->
--   -- | Value to compare on
--   (a -> b) ->
--   -- | Tree to dissect
--   Tree e (Info c a) ->
--   Map (Bipartition b) c
-- bipartitionToBranchLengthUnsafe (l, p) f (Node (l', p') []) =
--   M.singleton (bpwith f p p') (l <> l')
-- -- The branch length has to be added for degree two nodes.
-- bipartitionToBranchLengthUnsafe (l, p) f (Node (l', _) [x]) =
--   bipartitionToBranchLengthUnsafe (l <> l', p) f x
-- -- Go through the list of children and combine each of them with the rest.
-- bipartitionToBranchLengthUnsafe (l, p) f t@(Node (l', p') xs) =
--   M.unionsWith (<>) $
--     M.singleton (bpwith f p p') (l <> l') :
--       [ bipartitionToBranchLengthUnsafe (mempty, lvs) f x
--         | (lvs, x) <- zip lvsOthers xs
--       ]
--   where
--     lvsOthers = subForestGetSubsets p (fmap snd t)

-- -- | Determine compatibility between an bipartition and a subset.
-- --
-- -- If both subsets of the bipartition share elements with the given subset, the
-- -- bipartition is incompatible with this subset. If all elements of the subset
-- -- are either not in the bipartition or mapping to one of the two subsets of the
-- -- bipartition, the bipartition and the subset are compatible.
-- --
-- -- See also 'ELynx.Data.Tree.Multipartition.compatible'.
-- bpcompatible :: (Show a, Ord a) => Bipartition a -> Set a -> Bool
-- -- compatible (Bipartition (l, r)) ss = sintersection l ss `sdisjoint` sintersection r ss
-- bpcompatible (Bipartition (l, r)) ss = S.null lOverlap || S.null rOverlap
--   where
--     lOverlap = S.intersection l ss
--     rOverlap = S.intersection r ss
