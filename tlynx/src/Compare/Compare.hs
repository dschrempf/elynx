{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  Compare.Compare
Description :  Compare two phylogenies
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Sep 19 15:01:52 2019.

-}

module Compare.Compare
  ( compareCmd
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8        as L
import           Data.List
import qualified Data.Map                          as M
import           Data.Monoid
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Tree
import           System.IO
import           Text.Printf

import           Compare.Options

import           ELynx.Data.Tree.Bipartition
import           ELynx.Data.Tree.BranchSupportTree
import           ELynx.Data.Tree.Distance
import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.NamedTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Tree
import           ELynx.Export.Tree.Newick          (toNewick)
import           ELynx.Import.Tree.Newick
import           ELynx.Tools.InputOutput

treesOneFile :: FilePath -> Compare (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
treesOneFile tf = do
  $(logInfo) $ T.pack $ "Parse file '" ++ tf ++ "'."
  ts <- liftIO $ parseFileWith manyNewick tf
  let n = length ts
  case compare n 2 of
    LT -> error "Not enough trees in file."
    GT -> error "Too many trees in file."
    EQ -> return (head ts, head . tail $ ts)

treesTwoFiles :: FilePath -> FilePath -> Compare (Tree (PhyloLabel L.ByteString), Tree (PhyloLabel L.ByteString))
treesTwoFiles tf1 tf2 = do
  $(logInfo) $ T.pack $ "Parse first tree file '" ++ tf1 ++ "'."
  t1 <- liftIO $ parseFileWith oneNewick tf1
  $(logInfo) $ T.pack $ "Parse second tree file '" ++ tf2 ++ "'."
  t2 <- liftIO $ parseFileWith oneNewick tf2
  return (t1, t2)

-- | More detailed comparison of two trees.
compareCmd :: Maybe FilePath -> Compare ()
compareCmd outFile = do
  a <- lift ask
  -- Determine output handle (stdout or file).
  let outFn = (++ ".out") <$> outFile
  outH <- outHandle "results" outFn
  -- liftIO $ hPutStrLn outH ""

  -- Read input.
  let inFiles = argsInFiles a
      nFiles  = length inFiles
  (t1, t2) <- case nFiles of
    1 -> treesOneFile (head inFiles)
    2 -> treesTwoFiles (head inFiles) (head . tail $ inFiles)
    _ -> error "Need two input files with one tree each or one input file with two trees."
  -- liftIO $ hPutStrLn outH ""

  liftIO $ hPutStrLn outH "Tree 1:"
  liftIO $ L.hPutStrLn outH $ toNewick t1
  liftIO $ hPutStrLn outH "Tree 2:"
  liftIO $ L.hPutStrLn outH $ toNewick t2
  liftIO $ hPutStrLn outH ""

  -- Check input.
  let lvs1  = leaves t1
      lvs2  = leaves t2
      lfns1 = map getName lvs1
      lfns2 = map getName lvs2
      s1    = S.fromList lfns1
      s2    = S.fromList lfns2
  if s1 == s2
    then liftIO $ hPutStrLn outH "Trees have the same set of leaf names."
    else liftIO $ hPutStrLn outH "Trees do not have the same set of leaf names."
  liftIO $ hPutStrLn outH ""

  -- Distances.
  let formatD str val = T.justifyLeft 25 ' ' str <> "  " <> val
  liftIO $ hPutStrLn outH "Distances."
  liftIO $ T.hPutStrLn outH $ formatD "Symmetric"
    (T.pack $ show $ symmetric t1 t2)
  liftIO $ T.hPutStrLn outH $ formatD "Branch score"
    (T.pack $ show $ branchScore t1 t2)
  liftIO $ T.hPutStrLn outH $ formatD "Incompatible split (0.9)"
    (T.pack $ show $ incompatibleSplits (collapse 0.9 t1) (collapse 0.9 t2))
  liftIO $ T.hPutStrLn outH $ formatD "Incompatible split (0.5)"
    (T.pack $ show $ incompatibleSplits (collapse 0.5 t1) (collapse 0.5 t2))
  liftIO $ T.hPutStrLn outH $ formatD "Incompatible split (0.1)"
    (T.pack $ show $ incompatibleSplits (collapse 0.1 t1) (collapse 0.1 t2))

  -- Bipartitions.
  let bp1 = bipartitions (fmap getName t1)
      bp2 = bipartitions (fmap getName t2)
      bp1Only = bp1 S.\\ bp2
      bp2Only = bp2 S.\\ bp1
  unless (S.null bp1Only)
    (do
        liftIO $ hPutStrLn outH ""
        liftIO $ hPutStrLn outH "Bipartitions in Tree 1 that are not in Tree 2."
        -- let bp1Strs = map (bphuman L.unpack . bpmap getName) (S.toList bp1Only)
        forM_ bp1Only (liftIO . hPutStrLn outH . bphuman L.unpack))
        -- let bp1Strs = map (bphuman L.unpack) (S.toList bp1Only)
        -- liftIO $ hPutStrLn outH $ intercalate "\n" bp1Strs)
  unless (S.null bp2Only)
    (do
        liftIO $ hPutStrLn outH ""
        liftIO $ hPutStrLn outH "Bipartitions in Tree 2 that are not in Tree 1."
        forM_ bp2Only (liftIO . hPutStrLn outH . bphuman L.unpack))

  -- Common bipartitions and their respective differences in branch lengths.
  liftIO $ hPutStrLn outH ""
  let bpCommon = bp1 `S.intersection` bp2
  if S.null bpCommon
    then liftIO $ hPutStrLn outH "There are no common bipartitions."
    else do
    let bpToBrLen1 = M.map getSum $ bipartitionToBranch getName (Sum . getLen) t1
        bpToBrLen2 = M.map getSum $ bipartitionToBranch getName (Sum . getLen) t2
    liftIO $ hPutStrLn outH "Common bipartitions and their respective differences in branch lengths."
    -- Header.
    liftIO $ hPutStrLn outH header
    forM_ bpCommon (liftIO . hPutStrLn outH . getCommonBpStr L.unpack bpToBrLen1 bpToBrLen2)
  liftIO $ hClose outH
  -- TODO. Plot using haskell-chart.
  -- See https://github.com/timbod7/haskell-chart/wiki.

header :: String
header = intercalate "  " $ cols ++ ["Bipartition"]
  where cols = map (T.unpack . T.justifyRight 12 ' ')
               [ "Length 1", "Length 2", "Delta", "Relative [%]"]

getCommonBpStr :: (Ord a, Fractional b, PrintfArg b)
               => (a -> String)
               -> M.Map (Bipartition a) b -> M.Map (Bipartition a) b
               -> Bipartition a -> String
getCommonBpStr f m1 m2 p = intercalate "  "
  [ printf "% 12.7f" l1
  , printf "% 12.7f" l2
  , printf "% 12.7f" d
  , printf "% 12.7f" rd
  , s ]
  where l1 = m1 M.! p
        l2 = m2 M.! p
        d  = l1 - l2
        rd = 2 * d / (l1 + l2)
        s  = bphuman f p
