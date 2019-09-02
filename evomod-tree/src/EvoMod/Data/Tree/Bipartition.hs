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
  ( -- * The 'Bipartition' data type.
    Bipartition ()
  , bp
  , bpmap
    -- * Working with 'Bipartition's.
  , bipartitions
  , bipartitionToBranch
  , multipartitions
  ) where

import           Data.List
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Tree

import           EvoMod.Data.Tree.Tree

-- | Bipartitions with 'Set.Set's, since order of elements within the leaf sets
-- is not important. Also the order of the two leaf sets of the bipartition is
-- not important (see 'Eq' instance definition).
newtype Bipartition a = Bipartition (S.Set a, S.Set a)

instance Show a => Show (Bipartition a) where
  show (Bipartition (x, y)) = "(" ++ showSet x ++ "|" ++ showSet y ++  ")"
    where showSet s = intercalate "," $ map show $ S.toList s

-- | Create a bipartition from two 'Set's.
bp :: Ord a => S.Set a -> S.Set a -> Bipartition a
bp x y = if x >= y
         then Bipartition (x, y)
         else Bipartition (y, x)

instance (Eq a) => Eq (Bipartition a) where
  Bipartition x == Bipartition y = x == y

instance (Ord a) => Ord (Bipartition a) where
  Bipartition x `compare` Bipartition y = x `compare` y

-- | Map a function over all elements in the 'Bipartition's.
bpmap :: (Ord a, Ord b) => (a -> b) -> Bipartition a -> Bipartition b
bpmap f (Bipartition (x, y)) = Bipartition (S.map f x, S.map f y)

-- XXX.
-- -- | Each node of a tree is root of a subtree. Set the node label to the leaves
-- -- of this subtree.
-- toLeavesTree :: Tree a -> Tree [a]
-- toLeavesTree (Node l []) = Node [l] []
-- toLeavesTree (Node _ xs) = Node (concatMap rootLabel xs') xs'
--   where xs' = map toLeavesTree xs

-- TODO: Documentation.
leavesSet :: Ord a => Tree a -> S.Set a
leavesSet = S.fromList . leaves

-- TODO: Documentation.
forestGetLeafSets :: (Ord a) => S.Set a -> [Tree a] -> (S.Set a, [S.Set a])
forestGetLeafSets lvsS xs = (lvsCh, lvsStemAndOthers)
  where
    nCh              = length xs
    lvsChs           = map leavesSet xs
    lvsCh            = foldl1 (<>) lvsChs
    lvsOthers        = [ S.unions $ lvsS : take i lvsChs ++ drop (i+1) lvsChs
                      | i <- [0 .. (nCh - 1)] ]
    lvsStemAndOthers = map (S.union lvsS) lvsOthers

-- | Get all bipartitions. XXX: This is slow at the moment, because 'leaves' is
-- called excessively.
bipartitions :: Ord a => Tree a -> S.Set (Bipartition a)
bipartitions = S.fromList . bipartitions' S.empty

-- XXX: A helper function could reduce redundancy a lot in the next functions.
-- bipartitionsThisNode :: Tree a -> [Bipartition a]
-- But:
-- 1. The calling function need to pass on the leaves of the other branches, and
--    so, they have to be recalculated.
-- 2. The unnecessary recalculation of leaves is fostered.
-- XXX: Use 'toLeaves'.
bipartitions' :: Ord a => S.Set a -> Tree a -> [Bipartition a]
bipartitions' _   (Node _ []    ) = []
bipartitions' lsC (Node _ [c]   ) = bipartitions' lsC c
bipartitions' lsC (Node _ xs    )
  -- It really sucks that we have to treat a bifurcating root separately. But
  -- that's just how it is.
  | S.null lsC && length xs == 2 =
    let l = head xs
        r = xs !! 1
        lsL = leavesSet l
        lsR = leavesSet r
    in bp lsL lsR : bipartitions' lsL r ++ bipartitions' lsR l
  | otherwise = bs ++ concat (zipWith bipartitions' lsOthers xs)
  where
    nChildren  = length xs
    lsChildren = map leavesSet xs
    lsOthers   = [ S.unions $ lsC : take i lsChildren ++ drop (i+1) lsChildren
                 | i <- [0 .. (nChildren - 1)] ]
    bs         = zipWith bp lsChildren lsOthers

-- | Each branch on a 'Tree' defines a unique 'Bipartition' of leaves. Convert a
-- tree into a 'Map' from each 'Bipartition' to the inducing branch. The
-- information about the branch is extracted from the nodes with a given
-- function. If the tree has degree 2 nodes, the branch values are combined (see
-- 'Monoid' constraint).
bipartitionToBranch :: (Ord a, Monoid b)
                    => (a -> b)      -- ^ Convert node to branch length
                    -> Tree a        -- ^ Tree to dissect
                    -> M.Map (Bipartition a) b
-- A leaf does not induce any bipartition.
bipartitionToBranch _ (Node _ [] ) = M.empty
-- If the stem at the root is split by degree two nodes, just go on and ignore
-- the branch information, because the stem does not induce any bipartition
-- anyways..
bipartitionToBranch f (Node _ [x]) = bipartitionToBranch f x
-- We have rose trees, so we need to through the list of children and combine
-- each of them with the rest.
bipartitionToBranch f (Node _ xs ) =
  M.unionsWith (<>) [ bipartitionToBranch' lvs mempty f x
                      | (lvs, x) <- zip leafSets xs ]
  where leafSets = snd $ forestGetLeafSets S.empty xs

-- TODO: Testing!
-- XXX. Can fold or traversable be used?
bipartitionToBranch' :: (Ord a, Monoid b)
                     => S.Set a     -- ^ Complementary set of leaves towards the stem
                     -> b             -- ^ Maybe we have to pass along some
                                      -- information from above (degree two
                                      -- nodes)
                     -> (a -> b)      -- ^ Extract information about branch from node
                     -> Tree a
                     -> M.Map (Bipartition a) b
bipartitionToBranch' lvsS br f (Node l xs  )
  | S.null lvsS   = error "bipartitionToBranch': no complementing leaf set."
  -- Leaf; return a singleton map; bipartition with the leaf and the rest of the tree.
  | null xs         = M.singleton (bp lvsS (S.singleton l)) (br <> f l)
  -- Pass the creation of the map entry on, but extend the branch.
  | length xs == 1  = bipartitionToBranch' lvsS (br <> f l) f (head xs)
  -- We have rose trees, so we need to through the list of children and combine
  -- each of them with the rest. Also, we use up the possible branch information
  -- 'br' and start afresh with 'mempty'.
  | otherwise       = M.insert (bp lvsS lvsCh) (br <> f l)
                      $ M.unions [ bipartitionToBranch' lvs mempty f x
                                   | (lvs, x) <- zip lvsStemAndOthers xs ]
  where
    nCh              = length xs
    lvsChs           = map leavesSet xs
    lvsCh            = foldl1 (<>) lvsChs
    lvsOthers        = [ S.unions $ lvsS : take i lvsChs ++ drop (i+1) lvsChs
                      | i <- [0 .. (nCh - 1)] ]
    lvsStemAndOthers = map (S.union lvsS) lvsOthers

-- XXX: Rename this function. It does not compute multipartitions, rather it
-- computes bipartitions, but merges leaves for multifurcations.
-- | Get all bipartitions, but combine leaves from multi-furcations. This is
-- useful to find incompatible splits. See 'incompatibleSplitsDistance'.
multipartitions :: Ord a => Tree a -> [Bipartition a]
-- Assume that a root node with three children actually corresponds to an
-- unrooted tree.
multipartitions (Node _ [a, b, c]) = bp lsA lsBC
                                     : bp lsB lsAC
                                     : bp lsC lsAB
                                     : multipartitions' lsBC a
                                     ++ multipartitions' lsAC b
                                     ++ multipartitions' lsAB c
  where
    lsA = leavesSet a
    lsB = leavesSet b
    lsC = leavesSet c
    lsAB = lsA `S.union` lsB
    lsAC = lsA `S.union` lsC
    lsBC = lsB `S.union` lsC
multipartitions n                  = multipartitions' S.empty n

multipartitions' :: Ord a => S.Set a -> Tree a -> [Bipartition a]
multipartitions' _   (Node _ []    ) = []
multipartitions' lsC (Node _ [c]   ) = multipartitions' lsC c
multipartitions' lsC (Node _ [l, r])
  | S.null lsC = let lsL = leavesSet l
                     lsR = leavesSet r
                   in bp lsL lsR : multipartitions' lsL r ++ multipartitions' lsR l
  | otherwise = let lsL = leavesSet l
                    lsR = leavesSet r
                    lsCL = lsL `S.union` lsC
                    lsCR = lsR `S.union` lsC
                in bp lsCL lsR : bp lsCR lsL :
                   multipartitions' lsCL r ++ multipartitions' lsCR l
multipartitions' lsC n
  | S.null lsC = []
  | otherwise = [ bp lsC $ leavesSet n ]
