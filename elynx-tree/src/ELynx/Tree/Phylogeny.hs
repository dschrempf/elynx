{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Phylogeny
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 16:08:54 2019.
--
-- The purpose of this module is to facilitate usage of 'Tree's in phylogenetic
-- analyses. A /phylogeny/ is a 'Tree' with unique leaf labels, and unordered
-- sub-forest.
--
-- Using the 'Tree' data type has some disadvantages.
--
-- 1. All trees are rooted. Unrooted trees can be treated with a rooted data
-- structure, as it is used here. However, some functions may be meaningless.
--
-- 2. Changing branch labels, node labels, or the topology of the tree are slow
-- operations, especially, when the changes are close to the leaves of the tree.
--
-- 3. Internally, the underlying 'Tree' data structure stores the sub-forest as
-- an ordered list. Hence, we have to do some tricks when comparing phylogenies
-- (see 'equal'), and comparison is slow.
--
-- 4. Uniqueness of the leaves is not ensured by the data type, but has to be
-- checked at runtime. Functions relying on the tree to have unique leaves do
-- perform this check, and return 'Left' with an error message, if the tree has
-- duplicate leaves.
--
-- Note: 'Tree's are rooted.
--
-- Note: 'Tree's encoded in Newick format correspond to rooted trees. By
-- convention only, a tree parsed from Newick format is usually thought to be
-- unrooted, when the root node is multifurcating and has three children. This
-- convention is not used here. Newick trees are just parsed as they are, and a
-- rooted tree is returned.
--
-- Unrooted trees with multifurcating root nodes can be rooted using 'outgroup'.
-- Note that a new root node is introduced in this case.
--
-- The roots of trees with bifurcating root nodes can be changed with 'rootAt'
-- or 'midpoint'; a list of all rooted trees is returned by 'roots'.
module ELynx.Tree.Phylogeny
  ( -- * Functions
    equal,
    intersect,
    bifurcating,
    outgroup,
    midpoint,
    roots,
    rootAt,

    -- * Branch labels
    Phylo (..),
    toPhyloTree,
    measurableToPhyloTree,
    supportedToPhyloTree,
    phyloToLengthTree,
    phyloToSupportTree,
    phyloToSupportTreeUnsafe,
    PhyloExplicit (..),
    toExplicitTree,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.List hiding (intersect)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tree.Bipartition
import ELynx.Tree.Length
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import ELynx.Tree.Support
import GHC.Generics

-- A faster check could probably be done using 'Ord' and sets. The leave set
-- could be precomputed.
-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
equal :: (Eq e, Eq a) => Tree e a -> Tree e a -> Bool
equal ~(Node brL lbL tsL) ~(Node brR lbR tsR) =
  (brL == brR)
    && (lbL == lbR)
    && (length tsL == length tsR)
    && all (`elem` tsR) tsL

-- | Compute the intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set.
--
-- Degree two nodes are pruned with 'prune'.
--
-- Return 'Left' if:
--
-- - the intersection of leaves is empty.
intersect ::
  (Semigroup e, Eq e, Ord a) => Forest e a -> Either String (Forest e a)
intersect ts
  | S.null lvsCommon = Left "intersect: Intersection of leaves is empty."
  | otherwise = case sequence [dropLeavesWith (predicate ls) t | (ls, t) <- zip leavesToDrop ts] of
    Nothing -> Left "intersect: A tree is empty."
    Just ts' -> Right ts'
  where
    -- Leaf sets.
    lvss = map (S.fromList . leaves) ts
    -- Common leaf set.
    lvsCommon = foldl1' S.intersection lvss
    -- Leaves to drop for each tree in the forest.
    leavesToDrop = map (S.\\ lvsCommon) lvss
    -- Predicate.
    predicate lvsToDr l = l `S.member` lvsToDr

-- | Check if a tree is bifurcating.
--
-- A Bifurcating tree only contains degree one (leaves) and degree three nodes
-- (internal bifurcating nodes).
bifurcating :: Tree e a -> Bool
bifurcating (Node _ _ []) = True
bifurcating (Node _ _ [x, y]) = bifurcating x && bifurcating y
bifurcating _ = False

-- I believe that manual treatment with 'outgroup' is preferable.

-- -- | Remove multifurcations.
-- --
-- -- A caterpillar like bifurcating structure is used to resolve all
-- -- multifurcations on a tree.
-- --
-- -- Multifurcating nodes are copied and branches are 'split'.
-- resolve :: Splittable e => Tree e a -> Tree e a
-- resolve t@(Node _ _ []) = t
-- resolve (Node br lb [x]) = Node br lb [resolve x]
-- resolve (Node br lb [x, y]) = Node br lb $ map resolve [x, y]
-- resolve (Node br lb (Node brL lbL xsL : xs)) = Node br lb [Node brL' lbL (map resolve xsL), Node brL' lb (map resolve xs)]
--   where brL' = split brL

-- | Resolve a multifurcating root using an outgroup.
--
-- A bifurcating root node with the provided label is introduced. The affected
-- branch is 'split'.
--
-- Note, the degree of the former root node is decreased by one.
--
-- If the root node is bifurcating, use 'rootAt'.
--
-- Return 'Left' if
--
-- - the root node is not multifurcating;
--
-- - the tree has duplicate leaves;
--
-- - the provided outgroup is not found on the tree or is polyphyletic.
outgroup :: (Semigroup e, Splittable e, Ord a) => Set a -> a -> Tree e a -> Either String (Tree e a)
outgroup _ _ (Node _ _ []) = Left "outgroup: Root node is a leaf."
outgroup _ _ (Node _ _ [_]) = Left "outgroup: Root node has degree two."
outgroup _ _ (Node _ _ [_, _]) = Left "outgroup: Root node is bifurcating."
outgroup o r t@(Node b l ts)
  | duplicateLeaves t = Left "outgroup: Tree has duplicate leaves."
  | otherwise = do
    bip <- bp o (S.fromList lvs S.\\ o)
    rootAt bip t'
  where
    lvs = leaves t
    (Node brO lbO tsO) = head ts
    -- Introduce a bifurcating root node.
    t' = Node b r [Node (split brO) lbO tsO, Node (split brO) l (tail ts)]

-- The 'midpoint' algorithm is pretty stupid because it calculates all rooted
-- trees and then finds the one minimizing the difference between the heights of
-- the left and right sub tree. Actually, one just needs to move left or right,
-- with the aim to minimize the height difference between the left and right sub
-- tree.

-- | Root tree at the midpoint.
--
-- Return 'Left' if
--
-- - the root node is not bifurcating.
midpoint :: (Semigroup e, Splittable e, HasLength e) => Tree e a -> Either String (Tree e a)
midpoint (Node _ _ []) = Left "midpoint: Root node is a leaf."
midpoint (Node _ _ [_]) = Left "midpoint: Root node has degree two."
midpoint t@(Node _ _ [_, _]) = getMidpoint <$> roots t
midpoint _ = Left "midpoint: Root node is multifurcating."

findMinIndex :: Ord a => [a] -> Int
findMinIndex (x : xs) = go (0, x) 1 xs
  where
    go (i, _) _ [] = i
    go (i, z) j (y : ys) = if z < y then go (i, z) (j + 1) ys else go (j, y) (j + 1) ys
findMinIndex [] = error "findMinIndex: Empty list."

getMidpoint :: HasLength e => [Tree e a] -> Tree e a
getMidpoint ts = case t of
  (Node br lb [l, r]) ->
    let hl = height l
        hr = height r
        dh = (hl - hr) / 2
     in Node
          br
          lb
          [ applyStem (modLen (subtract dh)) l,
            applyStem (modLen (+ dh)) r
          ]
  -- Explicitly use 'error' here, because roots is supposed to return trees with
  -- bifurcating root nodes.
  _ -> error "getMidpoint: Root node is not bifurcating."
  where
    dhs = map getDeltaHeight ts
    i = findMinIndex dhs
    t = ts !! i

-- find index of minimum; take this tree and move root to the midpoint of the branch

-- Get delta height of left and right sub tree.
getDeltaHeight :: HasLength e => Tree e a -> Length
getDeltaHeight (Node _ _ [l, r]) = abs $ height l - height r
-- Explicitly use 'error' here, because roots is supposed to return trees with
-- bifurcating root nodes.
getDeltaHeight _ = error "getDeltaHeight: Root node is not bifurcating."

-- | For a rooted tree with a bifurcating root node, get all possible rooted
-- trees.
--
-- The root node (label and branch) is moved.
--
-- For a tree with @l=2@ leaves, there is one rooted tree. For a bifurcating
-- tree with @l>2@ leaves, there are @(2l-3)@ rooted trees. For a general tree
-- with a bifurcating root node, and a total number of @n>2@ nodes, there are
-- (n-2) rooted trees.
--
-- Moving a multifurcating root node to another branch would change the degree
-- of the root node. Hence, a bifurcating root is required. To resolve a
-- multifurcating root, please use 'outgroup'.
--
-- Connect branches according to the provided 'Semigroup' instance.
--
-- Upon insertion of the root, split the affected branch into one out of two
-- equal entities according to a given function.
--
-- Return 'Left' if the root node is not 'bifurcating'.
roots :: (Semigroup e, Splittable e) => Tree e a -> Either String (Forest e a)
roots (Node _ _ []) = Left "roots: Root node is a leaf."
roots (Node _ _ [_]) = Left "roots: Root node has degree two."
roots t@(Node b c [tL, tR]) = Right $ t : descend b c tR tL ++ descend b c tL tR
roots _ = Left "roots: Root node is multifurcating."

complementaryForests :: Tree e a -> Forest e a -> [Forest e a]
complementaryForests t ts = [t : take i ts ++ drop (i + 1) ts | i <- [0 .. (n -1)]]
  where
    n = length ts

-- From the bifurcating root, descend into one of the two pits.
--
-- descend splitFunction rootBranch rootLabel complementaryTree downwardsTree
descend :: (Semigroup e, Splittable e) => e -> a -> Tree e a -> Tree e a -> Forest e a
descend _ _ _ (Node _ _ []) = []
descend brR lbR tC (Node brD lbD tsD) =
  [ Node brR lbR [Node (split brDd) lbD f, Node (split brDd) lbDd tsDd]
    | (Node brDd lbDd tsDd, f) <- zip tsD cfs
  ]
    ++ concat
      [ descend brR lbR (Node (split brDd) lbD f) (Node (split brDd) lbDd tsDd)
        | (Node brDd lbDd tsDd, f) <- zip tsD cfs
      ]
  where
    brC' = branch tC <> brD
    tC' = tC {branch = brC'}
    cfs = complementaryForests tC' tsD

-- | Root a tree at a specific position.
--
-- Root the tree at the branch defined by the given bipartition. The original
-- root node is moved to the new position.
--
-- The root node must be bifurcating (see 'roots' and 'outgroup').
--
-- Connect branches according to the provided 'Semigroup' instance.
--
-- Upon insertion of the root, split the affected branch according to the
-- provided 'Splittable' instance.
--
-- Return 'Left', if:
--
-- - the root node is not bifurcating;
--
-- - the tree has duplicate leaves;
--
-- - the bipartition does not match the leaves of the tree.
rootAt ::
  (Semigroup e, Splittable e, Eq a, Ord a) =>
  Bipartition a ->
  Tree e a ->
  Either String (Tree e a)
rootAt b t
  -- Tree is checked for being bifurcating in 'roots'.
  --
  -- Do not use 'duplicateLeaves' here, because we also need to compare the leaf
  -- set with the bipartition.
  | length lvLst /= S.size lvSet = Left "rootAt: Tree has duplicate leaves."
  | toSet b /= lvSet = Left "rootAt: Bipartition does not match leaves of tree."
  | otherwise = rootAt' b t
  where
    lvLst = leaves t
    lvSet = S.fromList $ leaves t

-- Assume the leaves of the tree are unique.
rootAt' ::
  (Semigroup e, Splittable e, Ord a) =>
  Bipartition a ->
  Tree e a ->
  Either String (Tree e a)
rootAt' b t = do
  ts <- roots t
  case find (\x -> Right b == bipartition x) ts of
    Nothing -> Left "rootAt': Bipartition not found on tree."
    Just t' -> Right t'

-- | Branch label for phylogenetic trees.
--
-- Branches may have a length and a support value.
data Phylo = Phylo
  { brLen :: Maybe Length,
    brSup :: Maybe Support
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData)

instance Semigroup Phylo where
  Phylo mBL mSL <> Phylo mBR mSR =
    Phylo
      (getSum <$> (Sum <$> mBL) <> (Sum <$> mBR))
      (getMin <$> (Min <$> mSL) <> (Min <$> mSR))

instance ToJSON Phylo

instance FromJSON Phylo

-- | Set all branch lengths and support values to 'Just' the value.
--
-- Useful to export a tree with branch lengths in Newick format.
toPhyloTree :: (HasLength e, HasSupport e) => Tree e a -> Tree Phylo a
toPhyloTree = first toPhyloLabel

toPhyloLabel :: (HasLength e, HasSupport e) => e -> Phylo
toPhyloLabel x = Phylo (Just $ getLen x) (Just $ getSup x)

-- | Set all branch lengths to 'Just' the values, and all support values to
-- 'Nothing'.
--
-- Useful to export a tree with branch lengths but without branch support values
-- to Newick format.
measurableToPhyloTree :: HasLength e => Tree e a -> Tree Phylo a
measurableToPhyloTree = first measurableToPhyloLabel

measurableToPhyloLabel :: HasLength e => e -> Phylo
measurableToPhyloLabel x = Phylo (Just $ getLen x) Nothing

-- | Set all branch lengths to 'Nothing', and all support values to 'Just' the
-- values.
--
-- Useful to export a tree with branch support values but without branch lengths
-- to Newick format.
supportedToPhyloTree :: HasSupport e => Tree e a -> Tree Phylo a
supportedToPhyloTree = first supportedToPhyloLabel

supportedToPhyloLabel :: HasSupport e => e -> Phylo
supportedToPhyloLabel x = Phylo Nothing (Just $ getSup x)

-- | If root branch length is not available, set it to 0.
--
-- Return 'Left' if any other branch length is unavailable.
phyloToLengthTree :: Tree Phylo a -> Either String (Tree Length a)
phyloToLengthTree =
  maybe (Left "phyloToLengthTree: Length unavailable for some branches.") Right
    . bitraverse brLen pure
    . cleanStemLength

cleanStemLength :: Tree Phylo a -> Tree Phylo a
cleanStemLength (Node (Phylo Nothing s) l f) = Node (Phylo (Just 0) s) l f
cleanStemLength t = t

-- | Set branch support values of branches leading to the leaves and of the root
-- branch to maximum support.
--
-- Return 'Left' if any other branch has no available support value.
phyloToSupportTree :: Tree Phylo a -> Either String (Tree Support a)
phyloToSupportTree t =
  maybe
    (Left "phyloToSupportTree: Support value unavailable for some branches.")
    Right
    $ bitraverse brSup pure $
      cleanLeafSupport m $
        cleanRootSupport m t
  where
    m = getMaxSupport t

-- | Set all unavailable branch support values to maximum support.
phyloToSupportTreeUnsafe :: Tree Phylo a -> Tree Support a
phyloToSupportTreeUnsafe t = cleanSupport m t
  where
    m = getMaxSupport t

-- If all branch support values are below 1.0, set the max support to 1.0.
getMaxSupport :: Tree Phylo a -> Support
getMaxSupport = fromJust . max (Just 1.0) . bimaximum . bimap brSup (const Nothing)

cleanRootSupport :: Support -> Tree Phylo a -> Tree Phylo a
cleanRootSupport maxSup (Node (Phylo b Nothing) l xs) = Node (Phylo b (Just maxSup)) l xs
cleanRootSupport _ t = t

cleanLeafSupport :: Support -> Tree Phylo a -> Tree Phylo a
cleanLeafSupport s (Node (Phylo b Nothing) l []) = Node (Phylo b (Just s)) l []
cleanLeafSupport s (Node b l xs) = Node b l $ map (cleanLeafSupport s) xs

cleanSupport :: Support -> Tree Phylo a -> Tree Support a
cleanSupport maxSup (Node (Phylo _ s) l xs) = Node (fromMaybe maxSup s) l $ map (cleanSupport maxSup) xs

-- | Explicit branch label with branch length and branch support value.
data PhyloExplicit = PhyloExplicit
  { sBrLen :: Length,
    sBrSup :: Support
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance Semigroup PhyloExplicit where
  PhyloExplicit bL sL <> PhyloExplicit bR sR = PhyloExplicit (bL + bR) (min sL sR)

instance HasLength PhyloExplicit where
  getLen = sBrLen
  setLen b pl = pl {sBrLen = b}
  modLen f (PhyloExplicit l s) = PhyloExplicit (f l) s

instance Splittable PhyloExplicit where
  split l = l {sBrLen = b'}
    where
      b' = sBrLen l / 2.0

instance HasSupport PhyloExplicit where
  getSup = sBrSup
  setSup s pl = pl {sBrSup = s}
  modSup f (PhyloExplicit l s) = PhyloExplicit l (f s)

instance ToJSON PhyloExplicit

instance FromJSON PhyloExplicit

-- | Conversion to a 'PhyloExplicit' tree.
--
-- See 'phyloToLengthTree' and 'phyloToSupportTree'.
toExplicitTree :: Tree Phylo a -> Either String (Tree PhyloExplicit a)
toExplicitTree t = do
  lt <- phyloToLengthTree t
  st <- phyloToSupportTree t
  case zipTreesWith PhyloExplicit const lt st of
    -- Explicit use of error, since this case should not happen.
    Nothing -> error "toExplicitTree: Can not zip two trees with the same topology."
    Just zt -> return zt
