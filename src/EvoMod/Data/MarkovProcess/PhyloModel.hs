{- |
Module      :  EvoMod.Data.MarkovProcess.PhyloModel
Description :  Phylogenetic model
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Fri Feb  1 12:43:06 2019.

A phylogenetic model is a complete description of the evolutionary process. At
the moment, it is either a mixture model or a plain substitution model, but more
complicated models may be added in the future.

-}

module EvoMod.Data.MarkovProcess.PhyloModel
  ( PhyloModel (..)
  ) where

import EvoMod.Data.MarkovProcess.MixtureModel
import EvoMod.Data.MarkovProcess.SubstitutionModel

data PhyloModel = PhyloMixtureModel MixtureModel | PhyloSubstitutionModel SubstitutionModel
