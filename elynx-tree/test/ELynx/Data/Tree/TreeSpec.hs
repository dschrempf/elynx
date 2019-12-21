{- |
Module      :  ELynx.Data.Tree.TreeSpec
Copyright   :  (c) Dominik Schrempf 2019
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Mon May  6 14:04:05 2019.

-}

module ELynx.Data.Tree.TreeSpec
  (spec
  ) where

import qualified Data.ByteString.Lazy.Char8           as L
import           Data.Maybe
import           Data.Tree
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      hiding (label)
import           Test.QuickCheck.Instances.Containers ()

import           ELynx.Data.Tree.MeasurableTree
import           ELynx.Data.Tree.PhyloTree
import           ELynx.Data.Tree.Tree
import           ELynx.Import.Tree.Newick             hiding (node)
import           ELynx.Tools.InputOutput              (parseByteStringWith)

node :: Int -> Tree Int
node n = Node n []

smallTree :: Tree Int
smallTree = Node 0 [node 1, node 2]

smallSubTree :: Tree Int
smallSubTree = Node 0 [node 1]

smallSubTreePruned :: Tree Int
smallSubTreePruned = node 1

sampleTreeBS :: L.ByteString
sampleTreeBS = L.pack "(Aeropyrum0:0.5478645225,(((((((((Arabidopsi:0.0701001024,Oryza_sati:0.0765988261):0.0309636193,Gymnosperm:0.0520325624):0.0338982245,Physcomitr:0.0768008916):0.0895714685,(Chlamydomo:0.1136227755,Dunaliella:0.1406347323):0.1117340620):0.0818876186,Rhodophyta:0.3405656487):0.0363527066,((((((Babesia_bo:0.1646969208,Theileria0:0.1519889486):0.1908081096,Plasmodium:0.3250696762):0.0637865908,(Toxoplasma:0.1153570425,Eimeria000:0.1671916078):0.0980136930):0.0518956330,Cryptospor:0.3175062809):0.1607708388,Ciliophora:0.5687502950):0.0624078848,(Phytophtho:0.2016424948,((Thalassios:0.1202730781,Phaeodacty:0.1290341329):0.1772775509,Phaeophyce:0.1989260715):0.0312359673):0.1154768302):0.0311952864):0.0149160316,(((((((((Candida_al:0.1027755272,Saccharomy:0.1190206560):0.1333487870,Neurospora:0.1977309079):0.0522926266,Schizosacc:0.2019603227):0.0567441011,(Cryptococc:0.1948614959,Ustilago_m:0.1564451295):0.0775729694):0.0323959951,Glomus_int:0.1573670796):0.0194701292,Chytridiom:0.2228415254):0.0384370601,Encephalit:1.4622174644):0.0416231688,(((Drosophila:0.2160627753,(Mammalians:0.1080484094,Tunicates0:0.1739253014):0.0289624371):0.0346633757,Hydrozoa00:0.2058137032):0.0480963050,Monosiga_b:0.3020637584):0.0654894239):0.0380915725,(Dictyostel:0.3453588998,Mastigamoe:0.3844779231):0.0478795653):0.0129578395):1.7592083381,((Archaeoglo:0.5402784445,Methanococ:0.4088567459):0.0993669265,Pyrococcus:0.4058713829):0.1734405968):0.2193511807,Pyrobaculu:0.7507718047):0.1646616482,Sulfolobus:0.5404967897);"


largeTree :: Tree (PhyloLabel L.ByteString)
largeTree = parseByteStringWith "Sample newick byte string" newick sampleTreeBS

subSampleLargeTree :: Tree (PhyloLabel L.ByteString)
subSampleLargeTree = fromJust $ subTree ((== 'P') . L.head . label) largeTree

prop_roots :: Tree a -> Bool
prop_roots t
  -- XXX: Skip not bifurcating trees. This is ugly, I know.
  | not $ bifurcating t   = True
  | length (leaves t) < 3 = length (roots t) == 1
  | otherwise             = length (roots t) == 2 * length (leaves t) - 3

prop_connect :: a -> Tree a -> Tree a -> Bool
prop_connect n l r
  -- XXX: Skip not bifurcating trees. This is ugly, I know.
  | not (bifurcating l) || not (bifurcating r)     = True
  | length (leaves l) < 3 || length (leaves r) < 3 = length (connect n l r) == 1
  | otherwise = length (connect n l r) == length (leaves l) * length (leaves r)

spec :: Spec
spec = do
  describe "subTree" $ do
    it "returns nothing if no leaf satisfies prediacte" $
      subTree (==3) smallTree `shouldBe` Nothing
    it "returns the correct subtree for a small example" $
      subTree (==1) smallTree `shouldBe` Just smallSubTree

  describe "pruneWith" $ do
    it "leaves a normal tree untouched" $
      pruneWith const largeTree `shouldBe` largeTree
    it "correctly prunes a small example" $
      pruneWith const smallSubTree `shouldBe` smallSubTreePruned
    it "leaves height constant for Measurable trees" $
      height (prune subSampleLargeTree) `shouldBe` height subSampleLargeTree

  describe "roots" $ do
    it "correctly handles leaves and cherries" $ do
      let tleaf   = Node 0 [] :: Tree Int
          tcherry = Node 0 [Node 1 [], Node 2 []] :: Tree Int
      roots tleaf `shouldBe` [tleaf]
      roots tcherry `shouldBe` [tcherry]
    it "correctly handles simple trees" $ do
      let simpleTre = Node "i" [ Node "j" [Node "x" [], Node "y" []], Node "z" [] ]
          simpleSol =
            [ Node "i" [ Node "j" [ Node "x" [] , Node "y" [] ]
                       , Node "z" [] ]
            , Node "i" [ Node "x" []
                       , Node "j" [ Node "y" [] , Node "z" [] ] ]
            , Node "i" [ Node "j" [ Node "x" [] , Node "z" [] ]
                       , Node "y" [] ] ]
      roots simpleTre `shouldBe` simpleSol
    modifyMaxSize (*100) $
      it "returns the correct number of rooted trees for arbitrary trees" $
      property (prop_roots :: (Tree Int -> Bool))

  describe "connect" $
    modifyMaxSize (*100) $
    it "returns the correct number of rooted trees for arbitrary trees" $
      property (prop_connect :: Int -> Tree Int -> Tree Int -> Bool)
