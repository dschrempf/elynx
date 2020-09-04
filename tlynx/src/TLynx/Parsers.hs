-- |
-- Module      :  TLynx.Parsers
-- Description :  Common parsers
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Apr 22 13:34:35 2020.
module TLynx.Parsers
  ( NewickFormat,
    newickFormat,
    newickHelp,
  )
where

import Data.List (intercalate)
import ELynx.Tree
  ( NewickFormat (..),
    description,
  )
import ELynx.Tools (allValues)
import Options.Applicative

-- | Parse 'NewickFormat'.
newickFormat :: Parser NewickFormat
newickFormat =
  option auto $
    long "newick-format"
      <> short 'f'
      <> metavar "FORMAT"
      <> value Standard
      <> help ("Newick tree format: " ++ nwlist ++ "; default: Standard; for detailed help, see 'tlynx --help'")
  where
    nwfs = map show (allValues :: [NewickFormat])
    nwlist = intercalate ", " (init nwfs) <> ", or " <> last nwfs

-- | Help for different 'NewickFormat's.
newickHelp :: [String]
newickHelp = map (toListItem . description) (allValues :: [NewickFormat])
  where
    toListItem = ("- Newick " ++)
