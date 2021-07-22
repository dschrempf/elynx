{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      :  ELynx.Tree.RootedSpec
-- Description :  Unit tests for ELynx.Tree.Rooted
-- Copyright   :  (c) Dominik Schrempf 2021
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon May  6 14:04:05 2019.
module ELynx.Tree.RootedSpec
  ( spec,
  )
where

import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Monoid
import Data.Proxy
import ELynx.ClassLaws
import ELynx.Tools
import ELynx.Tree
import ELynx.Tree.Arbitrary ()
import Test.Hspec
-- import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (labels)
import Test.QuickCheck.Classes

node :: Int -> Tree () Int
node n = Node () n []

smallTree :: Tree () Int
smallTree = Node () 0 [node 1, node 2]

smallSubTree :: Tree () Int
smallSubTree = Node () 0 [node 1]

smallSubTreePruned :: Tree () Int
smallSubTreePruned = node 1

sampleTreeBS :: BL.ByteString
sampleTreeBS = "(Aeropyrum0:0.5478645225,(((((((((Arabidopsi:0.0701001024,Oryza_sati:0.0765988261):0.0309636193,Gymnosperm:0.0520325624):0.0338982245,Physcomitr:0.0768008916):0.0895714685,(Chlamydomo:0.1136227755,Dunaliella:0.1406347323):0.1117340620):0.0818876186,Rhodophyta:0.3405656487):0.0363527066,((((((Babesia_bo:0.1646969208,Theileria0:0.1519889486):0.1908081096,Plasmodium:0.3250696762):0.0637865908,(Toxoplasma:0.1153570425,Eimeria000:0.1671916078):0.0980136930):0.0518956330,Cryptospor:0.3175062809):0.1607708388,Ciliophora:0.5687502950):0.0624078848,(Phytophtho:0.2016424948,((Thalassios:0.1202730781,Phaeodacty:0.1290341329):0.1772775509,Phaeophyce:0.1989260715):0.0312359673):0.1154768302):0.0311952864):0.0149160316,(((((((((Candida_al:0.1027755272,Saccharomy:0.1190206560):0.1333487870,Neurospora:0.1977309079):0.0522926266,Schizosacc:0.2019603227):0.0567441011,(Cryptococc:0.1948614959,Ustilago_m:0.1564451295):0.0775729694):0.0323959951,Glomus_int:0.1573670796):0.0194701292,Chytridiom:0.2228415254):0.0384370601,Encephalit:1.4622174644):0.0416231688,(((Drosophila:0.2160627753,(Mammalians:0.1080484094,Tunicates0:0.1739253014):0.0289624371):0.0346633757,Hydrozoa00:0.2058137032):0.0480963050,Monosiga_b:0.3020637584):0.0654894239):0.0380915725,(Dictyostel:0.3453588998,Mastigamoe:0.3844779231):0.0478795653):0.0129578395):1.7592083381,((Archaeoglo:0.5402784445,Methanococ:0.4088567459):0.0993669265,Pyrococcus:0.4058713829):0.1734405968):0.2193511807,Pyrobaculu:0.7507718047):0.1646616482,Sulfolobus:0.5404967897);"

largeTree :: Tree Phylo Name
largeTree = parseByteStringWith (newick Standard) sampleTreeBS

subSampleLargeTree :: Tree Phylo Name
subSampleLargeTree = fromJust $ dropLeavesWith ((/= 'P') . BL.head . fromName) largeTree

prop_BranchTree_fmap :: (Eq e, Eq f) => (e -> f) -> Tree e e -> Bool
prop_BranchTree_fmap f t = first f t == getBranchTree (f <$> BranchTree t)

-- Check that zipping works the same for both instances ZipTree and
-- ZipBranchTree. However, this check does not verify that either works
-- correctly :).
prop_zip :: (Monoid e, Eq e, Eq a) => Tree e a -> Bool
prop_zip t = flipLabels (getZipBranchTree zbt') == getZipTree znt'
  where
    zbt = ZipBranchTree $ flipLabels t
    zbt' = (,) <$> zbt <*> zbt
    znt = ZipTree t
    znt' = (,) <$> znt <*> znt

-- Check that the Traversable instances of Tree and BranchTree work the same. I
-- am pretty confident that the Traversable instance of Tree is correct, so this
-- should be enough.
prop_BranchTree_traversable :: Eq e => Tree e a -> Bool
prop_BranchTree_traversable t = identify t == bt
  where
    bt = flipLabels $ getBranchTree $ identify $ BranchTree $ flipLabels t

-- Same as above but for zip trees.
prop_traversable_zip :: Eq e => Tree e a -> Bool
prop_traversable_zip t = (t' == zbt) && (t' == znt)
  where
    t' = identify t
    zbt = flipLabels $ getZipBranchTree $ identify $ ZipBranchTree $ flipLabels t
    znt = getZipTree $ identify $ ZipTree t

type T = Tree String Double

type BT = BranchTree String Double

type ZT = ZipTree String Double

type ZBT = ZipBranchTree String Double

spec :: Spec
spec = do
  describe "prune" $ do
    it "leaves a normal tree untouched" $
      prune largeTree `shouldBe` largeTree
    it "correctly prunes a small example" $
      prune smallSubTree `shouldBe` smallSubTreePruned
    it "leaves height constant for trees with branch lengths" $ do
      let t' =
            either error id $
              toLengthTree subSampleLargeTree
      height (prune t') `shouldBe` height t'
  describe "dropLeavesWith" $ do
    it "returns the same tree if no leaves satisfy predicate" $
      dropLeavesWith (const False) smallTree `shouldBe` Just smallTree
    it "returns nothing if all leaves satisfy predicate" $
      dropLeavesWith (const True) smallTree `shouldBe` Nothing
    it "returns the correct subtree for a small example" $
      dropLeavesWith (== 2) smallTree `shouldBe` Just smallSubTree
  describe "Tree" $ do
    it "has reasonable applicative take right instance" $
      property (prop_appl_right :: T -> T -> Bool)
    it "has reasonable applicative take left instance" $
      property (prop_appl_left :: T -> T -> Bool)
    it "has reasonable applicative liftA2 instance" $
      property (prop_appl (*) :: T -> Bool)
    it "has reasonable applicative and functor instances" $
      property (prop_appl_func (+ 3) :: T -> Bool)
  describe "BranchTree" $ do
    it "treats branches and labels correctly" $
      property (prop_BranchTree_fmap (* 2) :: Tree Double Double -> Bool)
    it "treats traversable instances equally" $
      property (prop_BranchTree_traversable :: Tree Int Int -> Bool)
    it "has reasonable applicative take right instance" $
      property (prop_appl_right :: BT -> BT -> Bool)
    it "has reasonable applicative take left instance" $
      property (prop_appl_left :: BT -> BT -> Bool)
    it "has reasonable applicative liftA2 instance" $
      property (prop_appl (*) :: BT -> Bool)
    it "has reasonable applicative and functor instances" $
      property (prop_appl_func (+ 3) :: BT -> Bool)
  describe "Zip trees" $ do
    it "treat branches and labels equally" $
      property (prop_zip :: Tree (Sum Int) Int -> Bool)
    it "treat traversable instances equally" $
      property (prop_traversable_zip :: Tree Int Int -> Bool)
    it "has reasonable applicative take right instance" $
      property (prop_appl_right :: ZT -> ZT -> Bool)
    it "has reasonable applicative take left instance" $
      property (prop_appl_left :: ZT -> ZT -> Bool)
    it "has reasonable applicative liftA2 instance" $
      property (prop_appl (*) :: ZT -> Bool)
    it "has reasonable applicative and functor instances" $
      property (prop_appl_func (+ 3) :: ZT -> Bool)
  describe "ZipBranch trees" $ do
    it "has reasonable applicative take right instance" $
      property (prop_appl_right :: ZBT -> ZBT -> Bool)
    it "has reasonable applicative take left instance" $
      property (prop_appl_left :: ZBT -> ZBT -> Bool)
    it "has reasonable applicative liftA2 instance" $
      property (prop_appl (*) :: ZBT -> Bool)
    it "has reasonable applicative and functor instances" $
      property (prop_appl_func (+ 3) :: ZBT -> Bool)
  -- TODO.
  describe "ZipTree" $
    it "follows common applicative laws" $
      lawsCheck (applicativeLaws (Proxy :: Proxy (ZipTree String)))
