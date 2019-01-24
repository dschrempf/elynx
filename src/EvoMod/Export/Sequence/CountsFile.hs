{- |
Description :  Write a counts file
Copyright   :  (c) Dominik Schrempf 2017
License     :  GPLv3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  non-portable (not tested)

After simulating data according to a tree and a mutation model (and a population
size, etc.), write the data to file using counts file format.

TODO: Add functions such as @[Sequence] -> B.ByteString@. But what do I actually
need? It only makes sense to do something like @[MulitSequenceAlignment] ->
B.ByteString@. But then I need to add positional information to
/MultiSequenceAlignment/.

-}

module EvoMod.Export.Sequence.CountsFile
  ( DataOneSite
  , PopulationNames
  , toCountsFile
  ) where

import qualified Data.ByteString.Char8                      as B
import           Data.Maybe                                 (fromMaybe)
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T

import           EvoMod.Data.Alphabet.BoundaryMutationModel

-- | The number of sites that will be printed.
type NSites = Int

-- | The names of the populations.
type PopulationNames = [T.Text]

-- Desired column width of the counts file.
colW :: Int
colW = 11

-- | Compose the header using the number of sites and the population names.
header :: NSites -> PopulationNames -> B.ByteString
header nSites popNames = T.encodeUtf8 $ T.unlines [lineOne, lineTwo]
  where nPop = length popNames
        lineOne = T.pack $ "COUNTSFILE NPOP " ++ show nPop ++ " NSITES " ++ show nSites
        lineTwo = T.unwords $
          [ T.justifyLeft colW ' ' $ T.pack "CHROM"
          , T.justifyLeft colW ' ' $ T.pack "POS" ]
          ++ map (T.justifyLeft colW ' ') popNames

-- | The chromosome name.
type Chrom = T.Text

-- | The position on the chromosome.
type Pos   = Int

-- | The set of boundary states for one site.
type DataOneSite = [State]

-- | Get a data line in the counts file.
dataLine :: Maybe Chrom -> Maybe Pos -> DataOneSite -> B.ByteString
dataLine chrom mPos bstates = T.encodeUtf8 $ T.unwords $
  [ T.justifyLeft colW ' ' (fromMaybe (T.pack "NA") chrom)
  , T.justifyRight colW ' ' (T.pack (maybe "NaN" show mPos)) ]
  ++ map (T.justifyRight colW ' ' . showCounts) bstates

-- | Convert data to a counts file.
toCountsFile :: PopulationNames -> [DataOneSite] -> B.ByteString
-- TODO: Chromosomal and positional information.
toCountsFile ns d = B.unlines $ header l ns : map (dataLine Nothing Nothing) d
  where l = length d
