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
  , chop
  , chop3
  ) where

import           Data.Function
import           Data.List

-- | Sort a list and also return original indices.
sortWithIndices :: Ord a => [a] -> [(a, Int)]
sortWithIndices xs = sortBy (compare `on` fst) $ zip xs ([0..] :: [Int])

-- | Chop list into chunks of given length. If the last chop is shorter than
-- length, it is dropped.
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs | length xs < n = []
          | otherwise     = take n xs : chop n (drop n xs)

-- | Chop list into tuples of length 3.
chop3 :: [a] -> [(a, a, a)]
chop3 xs | length xs < 3 = []
         | otherwise     = map (\[x,y,z] -> (x, y, z)) $ chop 3 xs
