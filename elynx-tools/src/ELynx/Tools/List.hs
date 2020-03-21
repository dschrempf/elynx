{- |
Module      :  ELynx.Tools.List
Description :  Additional tools for lists
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu May  2 18:57:39 2019.

-}

module ELynx.Tools.List
  ( -- * Lists
    sortListWithIndices
  , randomInsertList
  )
where

import           Control.Monad.Primitive
import           Data.Function
import           Data.List
import           System.Random.MWC

-- | Sort a list and also return original indices.
sortListWithIndices :: Ord a => [a] -> [(a, Int)]
sortListWithIndices xs = sortBy (compare `on` fst) $ zip xs ([0 ..] :: [Int])

-- | Insert element into random position of list.
randomInsertList :: PrimMonad m => a -> [a] -> Gen (PrimState m) -> m [a]
randomInsertList e v g = do
  let l = length v
  i <- uniformR (0, l) g
  return $ take i v ++ [e] ++ drop i v
