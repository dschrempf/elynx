{- |
Module      :  Evol.Tools
Description :  Auxiliary tools.
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Oct  5 13:27:56 2018.

-}


module Evol.Tools
  ( alignRight
  , alignLeft
  , allEqual
  ) where

alignRight :: Int -> String -> String
alignRight n s | l > n     = take n s
               | otherwise = replicate (n-l) ' ' ++ s
               where l = length s

alignLeft :: Int -> String -> String
alignLeft n s | l > n     = take n s
              | otherwise = s ++ replicate (n-l) ' '
               where l = length s

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)
