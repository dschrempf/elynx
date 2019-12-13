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

TODO: Strictly distinguish ROOTED / UNROOTED. It doesn't make sense to assume
that a tree is unrooted when the root is a trifurcation. Rather, I have to use
two data types.

TODO: The functions bipartitions' and friends seem to be bogus. To me it seems
that recursive bipartitionsUnsafe and friends should be all I need.

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
  , bipartitionsCompatible
  , bipartitionToBranch
  ) where

import           Data.Foldable             (foldr')
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
bipartitions t = if S.size (S.fromList lvs) == length lvs
                 then bipartitionsUnsafe t
                 else error "bipartitions: The tree contains duplicate leaves."
  where lvs = leaves t

-- | See 'bipartitions', but do not check if leaves are unique.
bipartitionsUnsafe :: Ord a => Tree a -> S.Set (Bipartition a)
bipartitionsUnsafe (Node _ [] ) = S.empty
-- If the root stem is split by degree two nodes, just go on since the root stem
-- does not induce any bipartitions.
bipartitionsUnsafe (Node _ [x]) = bipartitionsUnsafe x
-- We have rose trees, so we need to through the list of children and combine
-- each of them with the rest.
bipartitionsUnsafe t =
  S.unions [ bipartitions' lvs x
           | (lvs, x) <- zip lvsOthers (subForest lvsTree) ]
  where
    lvsTree = partitionTree t
    lvsOthers = subForestGetPartitions pempty lvsTree

-- The actual recursive worker function calculating the bipartition. Assume that
-- the root node has been handled adequately with 'bipartitions'.
bipartitions' :: Ord a => Partition a -> Tree (Partition a) -> S.Set (Bipartition a)
bipartitions' lvsStem t@(Node lvs xs)
  | pnull lvsStem = error "bipartitions': no complementing leaf set."
  -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
  | null xs        = S.singleton $ bp lvsStem lvs
  -- For degree two nodes, pass the creation of the set on.
  | length xs == 1 = bipartitions' lvsStem (head xs)
  -- We have rose trees, so we need to through the list of children and combine
  -- each of them with the rest.
  | otherwise      = S.unions $ S.singleton (bp lvsStem lvs) : zipWith bipartitions' lvsOthers xs
  where
    lvsOthers = subForestGetPartitions lvsStem t

-- TODO: Move documentation to Distance.hs?

-- | A multifurcation on a tree may (but not necessarily does) represent missing
-- information about the order of bifurcations. In this case, it is interesting
-- to get a set of compatible bifurcations of the tree. For example, the tree
--
-- > (A,(B,C,D))
--
-- induces the following bipartitions:
--
-- > A|BCD
-- > B|ACD
-- > C|ABD
-- > D|ABC
--
-- Those are also reported by 'bipartitions'. However, it is additionally compatible with
--
-- > AB|CD
-- > AC|BD
-- > AD|BC
--
-- 'bipartitionsCompatible' returns all of these bipartitions.
bipartitionsCompatible :: Ord a => Tree a -> S.Set (Bipartition a)
bipartitionsCompatible t = if S.size (S.fromList lvs) == length lvs
                           then bipartitionsCompatibleUnsafe pempty (partitionTree t)
                           else error "bipartitionsCompatible: The tree contains duplicate leaves."
  where lvs = leaves t

-- TODO: Remove; THIS IS TOO SLOW.

-- Get all partitions of a list of Monoids. Inspired by the partitions function
-- of the combinatorial package available at
-- https://hackage.haskell.org/package/combinatorial-0.1.0.1.
partitions :: Monoid a => [a] -> [(a, a)]
partitions = foldr'
             (\xs -> concatMap (\(lxs, rxs) -> [(xs <> lxs, rxs), (lxs, xs <> rxs)]))
             [(mempty, mempty)]

-- Get all bipartitions of a list of partitions. This is true Haskell beauty!
allBipartitions :: Ord a => [Partition a] -> S.Set (Bipartition a)
allBipartitions xs = S.fromList $ filter valid $ map (uncurry bp) ps
  where ps = partitions xs

-- | See 'bipartitionsCompatible', but do not check if leaves are unique.
bipartitionsCompatibleUnsafe :: Ord a => Partition a -> Tree (Partition a) -> S.Set (Bipartition a)
bipartitionsCompatibleUnsafe lvs   (Node lvsN [] ) = S.singleton $ bp lvs lvsN
-- If the root stem is split by degree two nodes, just go on since the root stem
-- does not induce any bipartitions.
bipartitionsCompatibleUnsafe lvs   (Node _    [c]) = bipartitionsCompatibleUnsafe lvs c
-- We have rose trees, so we need to through the list of children and combine
-- each of them with the rest.
bipartitionsCompatibleUnsafe lvs t@(Node _    cs ) = S.unions $
  allBipartitions (lvs : map rootLabel cs) :
  zipWith bipartitionsCompatibleUnsafe lvsOthers cs
  where lvsOthers = subForestGetPartitions lvs t

-- TODO: Remove?

-- -- The actual recursive worker function calculating the compatible bipartition.
-- -- Assume that the root node has been handled adequately with
-- -- 'bipartitionsCompatible'.
-- bipartitionsCompatible' :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Bipartition a)
-- bipartitionsCompatible' lvsStem t@(Node lvs xs)
--   | S.null lvsStem = error "bipartitionsCompatible': no complementing leaf set."
--   -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
--   | null xs        = S.singleton $ bp lvsStem lvs
--   -- For degree two nodes, pass the creation of the set on.
--   | length xs == 1 = bipartitionsCompatible' lvsStem (head xs)
--   -- Rose trees are compatible with all combinations of leaf sets.
--   | otherwise      = S.unions $ S.singleton (bp lvsStem lvs) : zipWith bipartitions' lvsOthers xs
--   where
--     lvsOthers = subForestGetLeafSets lvsStem t


-- | Each branch on a 'Tree' defines a unique 'Bipartition' of leaves. Convert a
-- tree into a 'M.Map' from each 'Bipartition' to the branch inducing the
-- respective 'Bipartition'. The information about the branch is extracted from
-- the nodes with a given function. If the tree has degree two nodes, the branch
-- values are combined; a unity element is required, and so we need the 'Monoid'
-- type class constraint. Checks if leaves are unique.
bipartitionToBranch :: (Ord a, Ord b, Monoid c)
                    => (a -> b)      -- ^ Value to compare on
                    -> (a -> c)      -- ^ Convert node to branch length
                    -> Tree a        -- ^ Tree to dissect
                    -> M.Map (Bipartition b) c
bipartitionToBranch f g t = if S.size (S.fromList lvs) == length lvs
                 then bipartitionToBranchUnsafe f g t
                 else error "bipartitionToBranch: The tree contains duplicate leaves."
  where lvs = leaves t

-- | See 'bipartitionToBranch', but does not check if leaves are unique.
bipartitionToBranchUnsafe :: (Ord a, Ord b, Monoid c)
                    => (a -> b)      -- ^ Value to compare on
                    -> (a -> c)      -- ^ Convert node to branch length
                    -> Tree a        -- ^ Tree to dissect
                    -> M.Map (Bipartition b) c
bipartitionToBranchUnsafe _ _ (Node _ [] ) = M.empty
-- If the root stem is split by degree two nodes, just go on and ignore the
-- branch information, because the stem does not induce any bipartition
-- anyways..
bipartitionToBranchUnsafe f g (Node _ [x]) = bipartitionToBranchUnsafe f g x
-- We have rose trees, so we need to through the list of children and combine
-- each of them with the rest.
bipartitionToBranchUnsafe f g t =
  M.unionsWith (<>) [ bipartitionToBranch' lvs mempty f g x
                    | (lvs, x) <- zip lvsOthers (subForest nodeAndLeavesTrees) ]
  where
    lvsTree            = partitionTree t
    nodeAndLeavesTrees = fromJust $ merge t lvsTree
    lvsOthers          = subForestGetPartitions pempty lvsTree

-- We need information about the nodes, and also about the leaves of the induced
-- sub trees. Hence, we need a somewhat complicated node type @(a, S.Set a)@.
bipartitionToBranch' :: (Ord a, Ord b, Monoid c)
                     => Partition a       -- ^ Complementary partition towards
                                          -- the stem
                     -> c                 -- ^ Maybe we have to pass along some
                                          -- information from above (degree two
                                          -- nodes)
                     -> (a -> b)          -- ^ Extract value to compare on
                     -> (a -> c)          -- ^ Extract information about branch
                                          -- from node
                     -> Tree (a, Partition a) -- ^ Tree containing nodes and sub
                                              -- tree leaf sets
                     -> M.Map (Bipartition b) c
bipartitionToBranch' lvs br f g t@(Node l xs )
  | pnull lvs       = error "bipartitionToBranch': no complementing leaf set."
  -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
  | null xs         = M.singleton (bpwith f lvs lvsThisNode) (br <> g label)
  -- Pass the creation of the map entry on, but extend the branch.
  | length xs == 1  = bipartitionToBranch' lvs (br <> g label) f g (head xs)
  -- We have rose trees, so we need to through the list of children and combine
  -- each of them with the rest. Also, we use up the possible branch information
  -- 'br' and start afresh with 'mempty'.
  | otherwise       = M.insert (bpwith f lvs lvsThisNode) (br <> g label)
                      $ M.unions [ bipartitionToBranch' ls mempty f g x
                                 | (ls, x) <- zip lvsOthers xs ]
  where
    label       = fst l
    lvsThisNode = snd l
    lvsOthers   = subForestGetPartitions lvs $ fmap snd t

-- TODO: Somehow this is wrong when used with multifurcating trees (i.e., the
-- incompatible split distance). Then, we need to distinguish between
--
-- 1. Resolved bipartitions that have to match with the other tree.
--
-- 2. Compatible bipartitions that are not fully resolved. They do not change
-- the distance if they are present in the other tree nor when they are missing
-- in the other tree.

-- TODO: Remove this, I think it is bogus.

-- -- | Get all bipartitions, but combine leaves from multifurcations. This is
-- -- useful to find incompatible splits. See
-- -- 'ELynx.Data.Tree.Distance.incompatibleSplitsDistance'. Assume that a root
-- -- node with three children is actually not a multifurcation (because then we
-- -- would have no induced bipartitions), but rather corresponds to an unrooted
-- -- tree.
-- --
-- -- XXX: It may be more advantageous to assume that the tree is rooted. What if
-- -- we want a multifurcation at the root?
-- bipartitionsCombined :: (Ord a, Show a) => Tree a -> S.Set (Bipartition a)
-- bipartitionsCombined t@(Node _ xs)
--   | null xs        = S.empty
--   | length xs == 1 = bipartitionsCombined (head xs)
--   -- One big multifurcation does not induce any bipartitions.
--   | length xs >  3 = S.empty
--   | otherwise      = res
--   where
--     res = S.unions [ bipartitionsCombined' lvs x
--                    | (lvs, x) <- zip lvsOthers (subForest lvsTree) ]
--     lvsTree = leavesTree t
--     lvsOthers = subForestGetLeafSets S.empty lvsTree

-- bipartitionsCombined' :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Bipartition a)
-- bipartitionsCombined' lvsStem t@(Node lvs xs)
--   | S.null lvsStem = error "bipartitionsCombined': no complementing leaf set."
--   | null xs        = S.singleton $ bp lvsStem lvs
--   | length xs == 1 = bipartitionsCombined' lvsStem (head xs)
--   | length xs == 2 = S.unions $
--                      S.singleton (bp lvsStem lvs) : zipWith bipartitionsCombined' lvsOthers xs
--   | otherwise      = S.singleton $ bp lvsStem lvs
--   where
--     lvsOthers = subForestGetLeafSets lvsStem t
