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

import Control.Monad
import Data.Containers.ListUtils (nubInt)
import qualified Data.Vector.Storable as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( EDMComponent,
    Parser,
  )
import ELynx.Tools
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer

-- | Parse stationary distributions from Phylobayes format.
siteprofiles :: Parser [EDMComponent]
siteprofiles = do
  _ <- headerLines
  cs <- many dataLine
  _ <- many newline *> eof <?> "phylobayes siteprofiles"
  let ls = map length cs
      nLs = length $ nubInt ls
  when
    (nLs /= 1)
    (error "The site profiles have a different number of entries.")
  return cs

horizontalSpace :: Parser ()
horizontalSpace = skipMany $ char (c2w ' ') <|> tab

line :: Parser ()
line = do
  _ <- many $ noneOf [c2w '\n']
  pure ()

-- For now, just ignore the header.
headerLines :: Parser ()
headerLines = do
  _ <- line
  _ <- many newline <?> "headerLine"
  pure ()

dataLine :: Parser EDMComponent
dataLine = do
  -- Ignore site number.
  _ <- decimal :: Parser Integer
  _ <- horizontalSpace
  -- Also ignore additional white space on line.
  vals <- float `sepEndBy1` horizontalSpace
  _ <- many newline <?> "dataLine"
  -- Set the weight to 1.0 for all sites.
  return (1.0, V.fromList vals)
