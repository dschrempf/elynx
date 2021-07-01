{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Phylogeny
-- Description :  Phylogenetic trees
-- Copyright   :  (c) Dominik Schrempf 2021
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
-- 2. Changing node labels, or the topology of the tree are slow operations,
-- especially, when the changes are close to the leaves of the tree.
--
-- 3. Internally, the underlying 'Tree' data structure stores the sub-forest as
-- an ordered list. Hence, we have to do some tricks when comparing phylogenies
-- (see 'equal'), and comparison is slow.
--
-- 4. Uniqueness of the leaves is not ensured by the data type, but has to be
-- checked at runtime. Functions relying on the tree to have unique leaves do
-- perform this check, and return 'Left' with a message, if the tree has
-- duplicate leaves.
--
-- NOTE: 'Tree's are rooted.
--
-- NOTE: 'Tree's encoded in Newick format correspond to rooted trees. By
-- convention only, a tree parsed from Newick format is usually thought to be
-- unrooted, when the root node is multifurcating and has three or more
-- children. This convention is not used here. Newick trees are just parsed as
-- they are, and a rooted tree is returned.
--
-- A multifurcating root node can be resolved to a bifurcating root node with
-- 'outgroup'.
--
-- The bifurcating root node can be changed with 'outgroup' or 'midpoint'.
--
-- For a given tree with bifurcating root node, a list of all rooted trees is
-- returned by 'roots'.
module ELynx.Tree.Phylogeny
  ( -- * Functions
    equal,
    equal',
    intersect,
    bifurcating,
    outgroup,
    midpoint,
    roots,

    -- * Branch labels
    Phylo (..),
    toPhyloTree,
    measurableToPhyloTree,
    toLengthTree,
    toSupportTree,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.List hiding (intersect)
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import ELynx.Tree.Bipartition
import ELynx.Tree.Length
import ELynx.Tree.Name
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import ELynx.Tree.Support
import GHC.Generics
import Lens.Micro

-- | The equality check is slow because the order of children is considered to
-- be arbitrary.
--
-- Return 'Left' if a tree does not have unique leaves.
equal :: (Eq a, Ord a) => Tree a -> Tree a -> Either String Bool
equal tL tR
  | duplicateLeaves tL = Left "equal: Left tree has duplicate leaves."
  | duplicateLeaves tR = Left "equal: Right tree has duplicate leaves."
  | otherwise = Right $ equal' tL tR

-- | Same as 'equal', but assume that leaves are unique.
equal' :: Eq a => Tree a -> Tree a -> Bool
equal' ~(Node lbL tsL) ~(Node lbR tsR) =
  (lbL == lbR)
    && (length tsL == length tsR)
    && all (elem' tsR) tsL
  where
    elem' ts t = isJust $ find (equal' t) ts

-- | Compute the intersection of trees.
--
-- The intersections are the largest subtrees sharing the same leaf set.
--
-- NOTE: Degree two nodes may arise. They can be pruned using 'pruneWith'.
--
-- Return 'Left' if:
--
-- - the intersection of leaves is empty.
intersect ::
  (Eq a, Ord a) => Forest a -> Either String (Forest a)
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
bifurcating :: Tree a -> Bool
bifurcating (Node _ []) = True
bifurcating (Node _ [x, y]) = bifurcating x && bifurcating y
bifurcating _ = False

-- | Root the tree using an outgroup.
--
-- NOTE: If the current root node is multifurcating, a bifurcating root node is
-- introduced by 'split'ting the leftmost branch. In this case, the 'Monoid' and
-- 'Splittable' instances of the node label are used, and the degree of the
-- former root node is decreased by one.
--
-- Given that the root note is bifurcating, the root node is moved to the
-- required position specified by the outgroup.
--
-- Branches are connected according to the provided 'Semigroup' instance.
--
-- Upon insertion of the root node at the required position, the affected branch
-- is 'split' according to the provided 'Splittable' instance.
--
-- Return 'Left' if
--
-- - the root node has degree two;
--
-- - the root node is a leaf;
--
-- - the tree has duplicate leaves;
--
-- - the provided outgroup is not found on the tree or is polyphyletic.
outgroup :: (HasMaybeLength a, HasName a) => Set a -> Tree a -> Either String (Tree a)
outgroup _ (Node _ []) = Left "outgroup: Root node is a leaf."
outgroup _ (Node _ [_]) = Left "outgroup: Root node has degree two."
outgroup o t@(Node _ [_, _]) = do
  bip <- bp o (S.fromList (leaves t) S.\\ o)
  rootAt bip t
outgroup o (Node lb ts) = outgroup o t'
  where
    (Node lbO tsO) = head ts
    -- Introduce a bifurcating root node.
    t' = Node mempty [Node (split lbO) tsO, Node (split brO) lb (tail ts)]

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
midpoint :: (Semigroup a, Splittable a, HasLength a) => Tree a -> Either String (Tree a)
midpoint (Node _ []) = Left "midpoint: Root node is a leaf."
midpoint (Node _ [_]) = Left "midpoint: Root node has degree two."
midpoint t@(Node _ [_, _]) = roots t >>= getMidpoint
midpoint _ = Left "midpoint: Root node is multifurcating."

-- Find the index of the smallest element.
findMinIndex :: Ord a => [a] -> Either String Int
findMinIndex (x : xs) = go (0, x) 1 xs
  where
    go (i, _) _ [] = Right i
    go (i, z) j (y : ys) = if z < y then go (i, z) (j + 1) ys else go (j, y) (j + 1) ys
findMinIndex [] = Left "findMinIndex: Empty list."

getMidpoint :: HasLength a => [Tree a] -> Either String (Tree a)
getMidpoint ts = case t of
  Right (Node lb [l, r]) ->
    let hl = height l
        hr = height r
        dh = (hl - hr) / 2
     in Right $
          Node
            lb
            [ l & labelL %~ modifyLength (subtract dh),
              r & labelL %~ modifyLength (+ dh)
            ]
  -- Explicitly use 'error' here, because roots is supposed to return trees with
  -- bifurcating root nodes.
  Right _ -> error "getMidpoint: Root node is not bifurcating; please contact maintainer."
  Left e -> Left e
  where
    dhs = map getDeltaHeight ts
    t = (ts !!) <$> findMinIndex dhs

-- find index of minimum; take this tree and move root to the midpoint of the branch

-- Get delta height of left and right sub tree.
getDeltaHeight :: HasLength a => Tree a -> Length
getDeltaHeight (Node _ [l, r]) = abs $ height l - height r
-- Explicitly use 'error' here, because roots is supposed to return trees with
-- bifurcating root nodes.
getDeltaHeight _ = error "getDeltaHeight: Root node is not bifurcating; please contact maintainer."

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
-- A bifurcating root is required because moving a multifurcating root node to
-- another branch would change the degree of the root node. To resolve a
-- multifurcating root, please use 'outgroup'.
--
-- Connect branches according to the provided 'Semigroup' instance.
--
-- Split the affected branch into one out of two equal entities according the
-- provided 'Splittable' instance.
--
-- Return 'Left' if the root node is not 'bifurcating'.
roots :: (Semigroup a, Splittable a) => Tree a -> Either String (Forest a)
roots (Node _ []) = Left "roots: Root node is a leaf."
roots (Node _ [_]) = Left "roots: Root node has degree two."
roots t@(Node c [tL, tR]) = Right $ t : descend c tR tL ++ descend c tL tR
roots _ = Left "roots: Root node is multifurcating."

complementaryForests :: Tree a -> Forest a -> [Forest a]
complementaryForests t ts = [t : take i ts ++ drop (i + 1) ts | i <- [0 .. (n -1)]]
  where
    n = length ts

-- From the bifurcating root, descend into one of the two pits.
--
-- descend rootLabel complementaryTree downwardsTree
descend :: (Semigroup a, Splittable a) => a -> Tree a -> Tree a -> Forest a
descend _ _ (Node _ []) = []
descend lbR tC (Node lbD tsD) =
  [ Node lbR [Node (split brDd) lbD f, Node (split brDd) lbDd tsDd]
    | (Node lbDd tsDd, f) <- zip tsD cfs
  ]
    ++ concat
      [ descend brR lbR (Node (split brDd) lbD f) (Node (split brDd) lbDd tsDd)
        | (Node lbDd tsDd, f) <- zip tsD cfs
      ]
  where
    lbC' = label tC <> label tD
    tC' = tC & labelL %~ brC'
    cfs = complementaryForests tC' tsD

-- Root a tree at a specific position.
--
-- Root the tree at the branch defined by the given bipartition. The original
-- root node is moved to the new position.
--
-- The root node must be bifurcating (see 'roots' and 'outgroup').
--
-- Return 'Left', if:
--
-- - the root node is not bifurcating;
--
-- - the tree has duplicate leaves;
--
-- - the bipartition does not match the leaves of the tree.
rootAt ::
  (Semigroup a, Splittable a, Eq a, Ord a) =>
  Bipartition a ->
  Tree a ->
  Either String (Tree a)
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
  (Semigroup a, Splittable a, Ord a) =>
  Bipartition a ->
  Tree a ->
  Either String (Tree a)
rootAt' b t = do
  ts <- roots t
  case find (\x -> bipartition x == Right b) ts of
    Nothing -> Left "rootAt': Bipartition not found on tree."
    Just t' -> Right t'

-- | Node label for phylogenetic trees.
--
-- The attached branch may have a length and a support value.
--
-- Especially useful to export trees to Newick format; see
-- 'ELynx.Tree.Export.Newick.toNewick'.
data Phylo = Phylo
  { pLength :: Maybe Length,
    pSupport :: Maybe Support,
    pName :: Name
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData)

-- | NOTE: The combining function is not commutative. The left name is favored
-- when combining two 'Phylo' labels.
instance Semigroup Phylo where
  Phylo mBL mSL lbN <> Phylo mBR mSR _ =
    Phylo
      (mBL <> mBR)
      (getMin <$> (Min <$> mSL) <> (Min <$> mSR))
      lbN

instance ToJSON Phylo

instance FromJSON Phylo

instance HasName Phylo where
  getName = getName . pName

-- | Set all branch lengths and support values to 'Just' the value.
toPhyloTree :: (HasMaybeLength a, HasMaybeSupport a, HasName a) => Tree a -> Tree Phylo
toPhyloTree = fmap toPhyloLabel

toPhyloLabel :: (HasMaybeLength a, HasMaybeSupport a, HasName a) => a -> Phylo
toPhyloLabel x = Phylo (getMaybeLength x) (getMaybeSupport x) (getName x)

-- | Set all branch lengths to 'Just' the values, and all support values to
-- 'Nothing'.
--
-- Useful to export a tree with branch lengths but without branch support values
-- to Newick format.
measurableToPhyloTree :: (HasMaybeLength a, HasName a) => Tree a -> Tree Phylo
measurableToPhyloTree = fmap measurableToPhyloLabel

measurableToPhyloLabel :: (HasMaybeLength a, HasName a) => a -> Phylo
measurableToPhyloLabel x = Phylo (getMaybeLength x) Nothing (getName x)

-- TODO: I need functions cleanLength, and cleanSupport.

-- | If root branch length is not available, set it to 0.
--
-- Return 'Left' if any other branch length is unavailable.
toLengthTree :: HasMaybeLength a => Tree a -> Either String (Tree Length)
toLengthTree =
  maybe (Left "toLengthTree: Length unavailable for some branches.") Right
    . traverse getMaybeLength
    . cleanStemLength

cleanStemLength :: HasMaybeSupport a => Tree a -> Tree a
cleanStemLength t = t & labelL %~ f
  where
    f x = case getMaybeLength x of
      Nothing -> setMaybeLength 0 x
      Just _ -> x

-- | Set branch support values of branches leading to the leaves and of the root
-- branch to maximum support.
--
-- Return 'Left' if any other branch has no available support value.
toSupportTree :: HasMaybeSupport a => Tree a -> Either String (Tree Support)
toSupportTree t =
  maybe (Left "phyloToSupportTree: Support value unavailable for some branches.") Right $
    traverse getMaybeSupport $
      cleanLeafSupport m $
        -- Clean root support value.
        cleanSupport m t
  where
    m = getMaxSupport t

-- If all branch support values are below 1.0, set the max support to 1.0.
getMaxSupport :: HasMaybeSupport a => Tree a -> Support
getMaxSupport = fromJust . max (Just 1.0) . maximum . fmap getMaybeSupport

cleanSupport :: HasMaybeSupport a => Support -> Tree a -> Tree a
cleanSupport maxSup t = t & labelL %~ f
  where
    f x = case getMaybeSupport x of
      Nothing -> setMaybeSupport maxSup x
      Just _ -> x

cleanLeafSupport :: HasMaybeSupport a => Support -> Tree a -> Tree a
cleanLeafSupport s l@(Node _ []) = cleanSupport s l
cleanLeafSupport s (Node lb xs) = Node lb $ map (cleanLeafSupport s) xs
