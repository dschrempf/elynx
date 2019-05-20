{- |
Module      :  EvoMod.Tools.List
Description :  Additional tools for lists
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May  2 18:57:39 2019.

-}

module EvoMod.Tools.List
  ( sortWithIndices
  ) where

import           Data.Function
import           Data.List

-- | Sort a list and also return original indices.
sortWithIndices :: Ord a => [a] -> [(a, Int)]
sortWithIndices xs = sortBy (compare `on` fst) $ zip xs ([0..] :: [Int])

