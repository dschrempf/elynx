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

module EvoMod.Export.CountsFile
  ( DataOneSite
  , PopulationNames
  , toCountsFile
  ) where

import qualified Data.ByteString.Lazy.Char8              as B
import           Data.Maybe                              (fromMaybe)
import           EvoMod.Data.Alphabet.BoundaryMutationModel
import           EvoMod.Tools                            (alignLeft, alignRight)

-- | The number of sites that will be printed.
type NSites = Int

-- | The names of the populations.
type PopulationNames = [String]

-- Desired column width of the counts file.
colW :: Int
colW = 11

-- | Compose the header using the number of sites and the population names.
header :: NSites -> PopulationNames -> B.ByteString
header nSites popNames = B.pack $ unlines [lineOne, lineTwo]
  where nPop = length popNames
        lineOne = "COUNTSFILE NPOP " ++ show nPop ++ " NSITES " ++ show nSites
        lineTwo = unwords $
          [ alignLeft colW "CHROM"
          , alignRight colW "POS" ]
          ++ map (alignRight colW) popNames

-- | The chromosome name.
type Chrom = String

-- | The position on the chromosome.
type Pos   = Int

-- | The set of boundary states for one site.
type DataOneSite = [State]

-- | Get a data line in the counts file.
dataLine :: Maybe Chrom -> Maybe Pos -> DataOneSite -> B.ByteString
dataLine chrom mPos bstates = B.pack $ unwords $
  [ alignLeft colW (fromMaybe "NA" chrom)
  , alignRight colW (maybe "NaN" show mPos) ]
  ++ map (alignRight colW . showCounts) bstates

-- | Convert data to a counts file.
toCountsFile :: PopulationNames -> [DataOneSite] -> B.ByteString
-- TODO: Chromosomal and positional information.
toCountsFile ns d = B.unlines $ header l ns : map (dataLine Nothing Nothing) d
  where l = length d
