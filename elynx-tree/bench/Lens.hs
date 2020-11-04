-- |
-- Module      :  Lens
-- Description :  Benchmark lens operations
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Nov  3 14:05:53 2020.
module Lens
  ( sumWithGetter,
    sumWithSetter,
    sumWithAccessorFunction,
    sumWithModifyFunction,
  )
where

import Data.Foldable
import ELynx.Tree.Measurable
import Lens.Micro

len :: Measurable a => Lens' a Length
len = lens getLen (flip setLen)

sumWithGetter :: Measurable a => [a] -> Length
sumWithGetter = foldl' (\x y -> x ^. len + y ^. len) 0

sumWithSetter :: Measurable a => [a] -> Length
sumWithSetter = sumWithGetter . map (\x -> x & len %~ (+ 10))

sumWithAccessorFunction :: Measurable a => [a] -> Length
sumWithAccessorFunction = foldl' (\x y -> getLen x + getLen y) 0

sumWithModifyFunction :: Measurable a => [a] -> Length
sumWithModifyFunction = sumWithAccessorFunction . map (modLen (+ 10))