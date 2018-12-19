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
'MultiSequenceAlignment'.

TODO: Parse Counts Files.

-}

module EvoMod.IO.CountsFile
  ( Pos
  , DataOneSite
  , NSites
  , PopulationNames
  , Chrom
  , toCountsFile
  ) where

import qualified Data.ByteString.Lazy.Char8              as B
import           EvoMod.Data.BoundaryMutationModel.State
-- import           EvoMod.Data.MultiSequenceAlignment
-- import           EvoMod.Data.Sequence
import           EvoMod.Tools                            (alignLeft, alignRight)

-- | The number of sites that will be printed.
type NSites = Int

-- | The names of the populations.
type PopulationNames = [String]

-- The column width of the counts file, take a save value. If the 'showCounts'
-- representation of a 'State' exceeds this value, there be dragons.
colW :: Int
colW = 15

-- | Compose the header using the number of sites and the population names.
header :: NSites -> PopulationNames -> B.ByteString
header nSites popNames = B.pack $ unlines [lineOne, lineTwo]
  where nPop = length popNames
        lineOne = "COUNTSFILE NPOP " ++ show nPop ++ " NSITES " ++ show nSites
        lineTwo = alignLeft colW "CHROM" ++ alignRight colW "POS" ++
          unwords (map (alignRight colW) popNames)

-- | The chromosome name.
type Chrom = String

-- | The position on the chromosome.
type Pos   = Int

-- | The set of boundary states for one site.
type DataOneSite = [State]

-- | Get a data line in the counts file.
dataLine :: Chrom -> Pos -> DataOneSite -> B.ByteString
dataLine chrom pos bstates = B.pack $
  alignLeft colW chrom ++ alignRight colW (show pos) ++ bStatesString ++ "\n"
  where bStatesString = unwords $ map (alignRight colW . showCounts) bstates

-- -- TODO: Information is missing. E.g., chromosome name. Conversion from Word8 to
-- -- State is ambiguous (population size is not know).
-- multiSequenceAlignmentToFasta :: MultiSequenceAlignment -> B.ByteString
-- multiSequenceAlignmentToFasta msa = B.unlines $ [ header l ns ] ++ map dataLine msa
--   where l  = msaLength msa
--         ns = map seqId $ msaSequences msa

-- TODO: Chromosomal and positional information.
toCountsFile :: NSites -> PopulationNames -> [DataOneSite] -> B.ByteString
toCountsFile l ns d = B.unlines $ header l ns : map (dataLine "NA" 0) d
