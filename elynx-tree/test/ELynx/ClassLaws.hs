-- |
-- Module      :  ELynx.ClassLaws
-- Description :  Unit tests for ELynx.ClassLaws
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Jul 22 20:39:58 2021.
module ELynx.ClassLaws
  ( prop_appl_right,
    prop_appl_left,
    prop_appl,
    prop_appl_func,
    filterLaws,
  )
where

import Control.Applicative
import ELynx.Tree.Arbitrary ()
import Test.QuickCheck.Classes

prop_appl_right :: (Applicative f, Eq (f a)) => f a -> f a -> Bool
prop_appl_right u v = (u *> v) == ((id <$ u) <*> v)

prop_appl_left :: (Applicative f, Eq (f a)) => f a -> f a -> Bool
prop_appl_left u v = (u <* v) == liftA2 const u v

prop_appl :: (Applicative f, Eq (f a)) => (a -> a -> a) -> f a -> Bool
prop_appl f x = liftA2 f x x == (f <$> x <*> x)

prop_appl_func :: (Applicative f, Eq (f b)) => (a -> b) -> f a -> Bool
prop_appl_func f x = fmap f x == (pure f <*> x)

filterLaws :: [String] -> Laws -> Laws
filterLaws xs (Laws tn ps) = Laws tn [ (n, p) | (n, p) <- ps, n `notElem` xs ]
