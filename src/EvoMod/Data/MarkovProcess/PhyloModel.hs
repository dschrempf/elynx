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
  , pmCode
  , pmSummarize
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8                  as L

import           EvoMod.Data.Alphabet.Alphabet
import           EvoMod.Data.MarkovProcess.MixtureModel
import           EvoMod.Data.MarkovProcess.SubstitutionModel

-- | A phylogenetic model is a mixture model or a substitution model. More
-- complicated models may be added.
data PhyloModel = PhyloMixtureModel MixtureModel | PhyloSubstitutionModel SubstitutionModel
  deriving (Show, Read)

-- | Extract code from phylogenetic model.
pmCode :: PhyloModel -> Code
pmCode (PhyloMixtureModel mm)      = getCodeMixtureModel mm
pmCode (PhyloSubstitutionModel sm) = sm ^. smCode

-- | Summarize a phylogenetic model; lines to be printed to screen or log.
pmSummarize :: PhyloModel -> [L.ByteString]
pmSummarize (PhyloMixtureModel mm)      = summarizeMixtureModel mm
pmSummarize (PhyloSubstitutionModel sm) = summarizeSubstitutionModel sm
