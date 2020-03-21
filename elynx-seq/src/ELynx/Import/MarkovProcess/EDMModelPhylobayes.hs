{- |
Module      :  ELynx.Import.MarkovProcess.EDMModelPhylobayes
Description :  Import stationary distributions from Phylobayes format
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Tue Jan 29 12:12:55 2019.

-}

module ELynx.Import.MarkovProcess.EDMModelPhylobayes
  ( Parser
  , EDMComponent
  , phylobayes
  )
where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.Vector.Storable          as V
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import           Text.Megaparsec.Byte.Lexer

import           ELynx.Data.MarkovProcess.EDMModel
import           ELynx.Tools

-- | Shortcut.
type Parser = Parsec Void L.ByteString

-- | Parse stationary distributions from Phylobayes format.
phylobayes :: Parser [EDMComponent]
phylobayes = do
  n  <- headerLine
  k  <- kComponentsLine
  cs <- count k $ dataLine n
  _  <- many newline *> eof <?> "phylobayes"
  return cs

horizontalSpace :: Parser ()
horizontalSpace = skipMany $ char (c2w ' ') <|> tab

headerLine :: Parser Int
headerLine = do
  n <- decimal
  _ <- horizontalSpace
  -- FIXME: This should be more general, but then we also want to ensure that
  -- the order of states is correct.
  _ <- chunk (L.pack "A C D E F G H I K L M N P Q R S T V W Y")
    <|> chunk (L.pack "A C G T")
  _ <- many newline <?> "headerLine"
  return n

kComponentsLine :: Parser Int
kComponentsLine = decimal <* newline <?> "kComponentsLine"

dataLine :: Int -> Parser EDMComponent
dataLine n = do
  weight <- float
  _      <- horizontalSpace
  vals   <- float `sepBy` horizontalSpace
  when (length vals /= n) (error "Did not find correct number of entries.")
  _ <- many newline <?> "dataLine"
  return (weight, V.fromList vals)
