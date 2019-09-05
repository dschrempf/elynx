{- |
Module      :  ELynx.Data.MarkovProcess.PhyloModel
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

To be imported qualified.

-}

module ELynx.Data.MarkovProcess.PhyloModel
  ( PhyloModel (..)
  , getAlphabet
  , summarize
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8                  as L

import           ELynx.Data.Alphabet.Alphabet
import qualified ELynx.Data.MarkovProcess.MixtureModel      as M
import qualified ELynx.Data.MarkovProcess.SubstitutionModel as S

-- | A phylogenetic model is a mixture model or a substitution model. More
-- complicated models may be added.
data PhyloModel = MixtureModel M.MixtureModel | SubstitutionModel S.SubstitutionModel
  deriving (Show, Read)

-- | Extract code from phylogenetic model.
getAlphabet :: PhyloModel -> Alphabet
getAlphabet (MixtureModel mm)      = M.getAlphabet mm
getAlphabet (SubstitutionModel sm) = sm ^. S.alphabet

-- | Summarize a phylogenetic model; lines to be printed to screen or log.
summarize :: PhyloModel -> [L.ByteString]
summarize (MixtureModel mm)      = M.summarize mm
summarize (SubstitutionModel sm) = S.summarize sm
