-- |
-- Module      :  ELynx.Tree.Strategies
-- Description :  Evaluation strategies for trees
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Sep  7 13:36:45 2020.
module ELynx.Tree.Strategies
  ( parTree,
    parTree2,
    parTree3,
    using,
  )
where

import Control.Parallel.Strategies
import ELynx.Tree.Rooted

-- | Evaluates all elements of the 'forest' in parallel.
parTree :: (NFData e, NFData a) => Strategy (Tree e a)
parTree (Node br lb ts) = Node <$> rdeepseq br <*> rdeepseq lb <*> parTraversable rdeepseq ts

-- | Evaluates all elements of the 'forest's of the 'forest' in parallel.
parTree2 :: (NFData e, NFData a) => Strategy (Tree e a)
parTree2 (Node br lb ts) = Node <$> rdeepseq br <*> rdeepseq lb <*> parTraversable parTree ts

-- | Evaluates all elements of the 'forest's of the 'forest's of the 'forest' in parallel.
parTree3 :: (NFData e, NFData a) => Strategy (Tree e a)
parTree3 (Node br lb ts) = Node <$> rdeepseq br <*> rdeepseq lb <*> parTraversable parTree2 ts
