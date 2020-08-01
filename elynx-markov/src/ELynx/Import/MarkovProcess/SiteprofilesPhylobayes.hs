-- |
-- Module      :  ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
-- Description :  Import site profiles in Phylobayes format
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 12:12:55 2019.
--
-- For now I just try to go with a huge empirical distribution mixture model. Let's
-- see if performance is good enough.
--
-- There are subtle differences between
-- `ELynx.Import.MarkovProcess.EDMModelPhylobayes` and this module, which collects
-- one stationary distribution for each site.
module ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
  ( siteprofiles,
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C
import Control.Monad
import Data.Containers.ListUtils (nubInt)
import qualified Data.Vector.Storable as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes

-- | Parse stationary distributions from Phylobayes format.
siteprofiles :: Parser [EDMComponent]
siteprofiles = (<?> "phylobayes siteprofiles") $ do
  _ <- headerLines
  cs <- many dataLine
  _ <- C.skipWhile C.isSpace
  _ <- endOfInput
  let ls = map length cs
      nLs = length $ nubInt ls
  when
    (nLs /= 1)
    (error "The site profiles have a different number of entries.")
  return cs

line :: Parser ()
line = skipWhile (not . C.isEndOfLine)

-- For now, just ignore the header.
headerLines :: Parser ()
headerLines = line *> C.skipWhile C.isSpace <?> "headerLine"

dataLine :: Parser EDMComponent
dataLine = (<?> "dataLine") $ do
  -- Ignore site number.
  _ <- C.decimal :: Parser Int
  _ <- skipWhile C.isHorizontalSpace
  -- Also ignore additional white space on line.
  vals <- C.double `sepBy1` skipWhile C.isHorizontalSpace
  _ <- C.skipWhile C.isSpace
  -- Set the weight to 1.0 for all sites.
  return (1.0, V.fromList vals)
