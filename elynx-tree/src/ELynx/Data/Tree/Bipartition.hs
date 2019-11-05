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
  , bp
  , bpmap
    -- * Working with 'Bipartition's.
  , bipartitions
  , bipartitionToBranch
  , bipartitionsCombined
  ) where

-- import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import           Data.Tree

import           ELynx.Data.Tree.Tree

-- | Bipartitions with 'S.Set's, since order of elements within the leaf sets
-- is not important. Also the order of the two leaf sets of the bipartition is
-- not important (see 'Eq' instance definition).
newtype Bipartition a = Bipartition (S.Set a, S.Set a)
  deriving (Show, Read)

-- XXX: At some point I should provide a readable show instance. In this case, I
-- need the following identity to hold:
-- > read . show = id
--
-- instance Show a => Show (Bipartition a) where
--   show (Bipartition (x, y)) = "(" ++ showSet x ++ "|" ++ showSet y ++  ")"
--     where showSet s = intercalate "," $ map show $ S.toList s

-- | Create a bipartition from two 'S.Set's.
bp :: Ord a => S.Set a -> S.Set a -> Bipartition a
bp x y = if x >= y
         then Bipartition (x, y)
         else Bipartition (y, x)

-- | Create a bipartition from two 'S.Set's.
bpWith :: (Ord a, Ord b) => (a -> b) -> S.Set a -> S.Set a -> Bipartition b
bpWith f x y = bpmap f $ bp x y

instance (Eq a) => Eq (Bipartition a) where
  Bipartition x == Bipartition y = x == y

instance (Ord a) => Ord (Bipartition a) where
  Bipartition x `compare` Bipartition y = x `compare` y

-- | Map a function over all elements in the 'Bipartition's.
bpmap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpmap f (Bipartition (x, y)) = bp (S.map f x) (S.map f y)

-- | Each node of a tree is root of a subtree. Get the leaves of the subtree of
-- each node.
leavesTree :: (Ord a) => Tree a -> Tree (S.Set a)
leavesTree (Node l []) = Node (S.singleton l) []
leavesTree (Node _ xs) = Node (S.unions $ map rootLabel xs') xs'
  where xs' = map leavesTree xs

-- | Loop through each tree in a forest to report the complementary leaf sets.
subForestGetLeafSets :: (Ord a)
                     => S.Set a          -- ^ Complementary leaf set at the stem
                     -> Tree (S.Set a)   -- ^ Tree with leaf set nodes
                     -> [S.Set a]
subForestGetLeafSets lvsS t = lvsOthers
  where
    xs               = subForest t
    nChildren        = length xs
    lvsChildren      = map rootLabel xs
    lvsOtherChildren = [ S.unions $ lvsS
                         : take i lvsChildren ++ drop (i+1) lvsChildren
                       | i <- [0 .. (nChildren - 1)] ]
    lvsOthers        = map (S.union lvsS) lvsOtherChildren


-- | Get all bipartitions.
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
    lvsTree = leavesTree t
    lvsOthers = subForestGetLeafSets S.empty lvsTree

bipartitions' :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Bipartition a)
bipartitions' lvsStem t@(Node lvs xs)
  | S.null lvsStem = error "bipartitions': no complementing leaf set."
  -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
  | null xs        = S.singleton $ bp lvsStem lvs
  -- For degree two nodes, pass the creation of the set on.
  | length xs == 1 = bipartitions' lvsStem (head xs)
  -- We have rose trees, so we need to through the list of children and combine
  -- each of them with the rest. Also, we use up the possible branch information
  -- 'br' and start afresh with 'mempty'.
  | otherwise      = S.unions $ S.singleton (bp lvsStem lvs) : zipWith bipartitions' lvsOthers xs
  where
    lvsOthers = subForestGetLeafSets lvsStem t

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
    lvsTree            = leavesTree t
    nodeAndLeavesTrees = fromJust $ merge t lvsTree
    lvsOthers          = subForestGetLeafSets S.empty lvsTree

-- We need information about the nodes, and also about the leaves of the induced
-- sub trees. Hence, we need a somewhat complicated node type @(a, S.Set a)@.
bipartitionToBranch' :: (Ord a, Ord b, Monoid c)
                     => S.Set a           -- ^ Complementary set of leaves
                                          -- towards the stem
                     -> c                 -- ^ Maybe we have to pass along some
                                          -- information from above (degree two
                                          -- nodes)
                     -> (a -> b)          -- ^ Extract value to compare on
                     -> (a -> c)          -- ^ Extract information about branch
                                          -- from node
                     -> Tree (a, S.Set a) -- ^ Tree containing nodes and sub
                                          -- tree leaf sets
                     -> M.Map (Bipartition b) c
bipartitionToBranch' lvsStem br f g t@(Node l xs )
  | S.null lvsStem  = error "bipartitionToBranch': no complementing leaf set."
  -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
  | null xs         = M.singleton (bpWith f lvsStem lvsThisNode) (br <> g label)
  -- Pass the creation of the map entry on, but extend the branch.
  | length xs == 1  = bipartitionToBranch' lvsStem (br <> g label) f g (head xs)
  -- We have rose trees, so we need to through the list of children and combine
  -- each of them with the rest. Also, we use up the possible branch information
  -- 'br' and start afresh with 'mempty'.
  | otherwise       = M.insert (bpWith f lvsStem lvsThisNode) (br <> g label)
                      $ M.unions [ bipartitionToBranch' lvs mempty f g x
                                 | (lvs, x) <- zip lvsOthers xs ]
  where
    label       = fst l
    lvsThisNode = snd l
    lvsOthers   = subForestGetLeafSets lvsStem $ fmap snd t

-- | Get all bipartitions, but combine leaves from multifurcations. This is
-- useful to find incompatible splits. See
-- 'ELynx.Data.Tree.Distance.incompatibleSplitsDistance'. Assume that a root
-- node with three children is actually not a multifurcation (because then we
-- would have no induced bypartitions), but rather corresponds to an unrooted
-- tree.
bipartitionsCombined :: (Ord a, Show a) => Tree a -> S.Set (Bipartition a)
bipartitionsCombined t@(Node _ xs)
  | null xs        = S.empty
  | length xs == 1 = bipartitionsCombined (head xs)
  -- One big multifurcation does not induce any bipartitions.
  | length xs >  3 = S.empty
  | otherwise      = res
  where
    res = S.unions [ bipartitionsCombined' lvs x
                   | (lvs, x) <- zip lvsOthers (subForest lvsTree) ]
    lvsTree = leavesTree t
    lvsOthers = subForestGetLeafSets S.empty lvsTree

bipartitionsCombined' :: Ord a => S.Set a -> Tree (S.Set a) -> S.Set (Bipartition a)
bipartitionsCombined' lvsStem t@(Node lvs xs)
  | S.null lvsStem = error "bipartitionsCombined': no complementing leaf set."
  | null xs        = S.singleton $ bp lvsStem lvs
  | length xs == 1 = bipartitionsCombined' lvsStem (head xs)
  | length xs == 2 = S.unions $
                     S.singleton (bp lvsStem lvs) : zipWith bipartitionsCombined' lvsOthers xs
  | otherwise      = S.singleton $ bp lvsStem lvs
  where
    lvsOthers = subForestGetLeafSets lvsStem t
