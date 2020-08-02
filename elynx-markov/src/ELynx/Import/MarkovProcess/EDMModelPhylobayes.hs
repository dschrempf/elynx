{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  ELynx.Import.MarkovProcess.EDMModelPhylobayes
-- Description :  Import stationary distributions from Phylobayes format
-- Copyright   :  (c) Dominik Schrempf 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Jan 29 12:12:55 2019.
module ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( Parser,
    EDMComponent,
    phylobayes,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector.Storable as V
import ELynx.Data.MarkovProcess.MixtureModel

-- | An empirical mixture model component has a weight and a stationary
-- distribution.
type EDMComponent = (Weight, V.Vector Double)

-- | Parse stationary distributions from Phylobayes format.
phylobayes :: Parser [EDMComponent]
phylobayes = (<?> "phylobayes") $ do
  n <- headerLine
  k <- kComponentsLine
  cs <- count k $ dataLine n
  _ <- C.skipWhile C.isSpace
  _ <- endOfInput
  return cs

headerLine :: Parser Int
headerLine = do
  n <- C.decimal
  _ <- skipWhile C.isHorizontalSpace
  -- XXX: This should be more general, but then we also want to ensure that the
  -- order of states is correct.
  _ <- string (BL.toStrict "A C D E F G H I K L M N P Q R S T V W Y")
      <|> string (BL.toStrict "A C G T")
  _ <- C.skipWhile C.isSpace <?> "headerLine"
  return n

kComponentsLine :: Parser Int
kComponentsLine = C.decimal <* C.skipWhile C.isSpace <?> "kComponentsLine"

dataLine :: Int -> Parser EDMComponent
dataLine n = (<?> "dataLine") $ do
  weight <- C.double
  _ <- skipWhile C.isHorizontalSpace
  vals <- C.double `sepBy1` skipWhile C.isHorizontalSpace
  _ <- C.skipWhile C.isSpace
  when (length vals /= n) (error "Did not find correct number of entries.")
  return (weight, V.fromList vals)
