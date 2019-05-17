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

import           EvoMod.Data.Alphabet.Character
import qualified EvoMod.Data.MarkovProcess.MixtureModel      as M
import qualified EvoMod.Data.MarkovProcess.SubstitutionModel as S

-- | A phylogenetic model is a mixture model or a substitution model. More
-- complicated models may be added.
data PhyloModel = PhyloMixtureModel M.MixtureModel | PhyloSubstitutionModel S.SubstitutionModel
  deriving (Show, Read)

-- | Extract code from phylogenetic model.
pmCode :: PhyloModel -> Code
pmCode (PhyloMixtureModel mm)      = M.getCodeMixtureModel mm
pmCode (PhyloSubstitutionModel sm) = sm ^. S.code

-- | Summarize a phylogenetic model; lines to be printed to screen or log.
pmSummarize :: PhyloModel -> [L.ByteString]
pmSummarize (PhyloMixtureModel mm)      = M.summarizeMixtureModel mm
pmSummarize (PhyloSubstitutionModel sm) = S.summarizeSubstitutionModel sm
