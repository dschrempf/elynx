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
  , randomInsert
  ) where

import           Control.Monad.Primitive
import           Data.Function
import           Data.List
import           System.Random.MWC

-- | Sort a list and also return original indices.
sortWithIndices :: Ord a => [a] -> [(a, Int)]
sortWithIndices xs = sortBy (compare `on` fst) $ zip xs ([0..] :: [Int])

-- | Insert element into random position of list.
randomInsert :: PrimMonad m => a -> [a] -> Gen (PrimState m) -> m [a]
randomInsert e v g = do
  let l = length v
  i <- uniformR (0, l) g
  return $ take i v ++ [e] ++ drop i v
