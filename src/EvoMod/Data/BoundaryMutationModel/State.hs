{- |
Description :  State space of the boundary mutation model
Copyright   :  (c) Dominik Schrempf 2017
License     :  GPLv3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  non-portable (not tested)

The boundary mutation model is a discrete-state, continuous-time Markov process
that allows mutations only when the population is monomorphic.

* Changelog

-}

module EvoMod.Data.BoundaryMutationModel.State
  ( -- * Types
    Allele
  , PopSize
  , AlleleCount
  , State(..)
  , nFixed
    -- * Functions
  , setPopSize
  , fromIndexWith
  , toIndex
  , stateSpace
  , stateSpaceSize
  -- , rmStateToBMState
  , neighbors
  ) where

import           Data.List

import           Control.Lens

import           Numeric.SpecFunctions  (choose)

import           EvoMod.Data.Character
import           EvoMod.Data.Nucleotide
-- import qualified RateMatrix as RM
import           EvoMod.Tools           (allValues)

-- | Alleles are just nucleotides at the moment. However, I want to keep the
-- code such that it can be extended easily to codons or amino acids.
type Allele = Nucleotide
-- XXX: Change N to D and PopSize to Discretization.
-- | The population size; has to be larger than one, otherwise there be dragons.
type PopSize = Int
-- | The absolute frequency of an allele.
type AlleleCount = Int

-- | The number of alleles.
nAlleles :: Int
nAlleles = 1 + fromEnum (maxBound :: Allele)

-- | A boundary mutation model state is either a boundary state or a polymorphic
-- state. This also automatically defines a total order.
--
-- Another possibility would be:
-- @
--  data State = Bnd Allele | Ply AlleleCount Allele Allele
--  data StateComplete = StateComplete PopSize State
-- @
-- But then, I think it is more important that the information is kept in one,
-- at the cost of some overhead.
data State = Bnd { bndN :: PopSize
                 , bndA :: Allele }
           | Ply { plyN :: PopSize
                 , plyI :: AlleleCount
                 , plyA :: Allele
                 , plyB :: Allele }
           deriving (Read, Eq)

showCounts :: State -> String
showCounts (Bnd n a) = foldl1' (++) $ intersperse "," $ map toCounts allValues
  where toCounts b
          | a == b    = show n
          | otherwise = "0"
showCounts (Ply n i a b) = foldl1' (++) $ intersperse "," $ map toCounts allValues
  where toCounts c
          | c == a    = show i
          | c == b    = show (n-i)
          | otherwise = "0"

instance Show State where
  show s = "(" ++ showCounts s ++ ")"

-- | A total order on the boundary mutation model states. In general, Bnd < Ply.
-- Then, sorting happens according to the order population size, first allele,
-- second allele, allele count. It may be beneficial to reverse the allele count
-- order (i.e., make a polymorphic state with higher allele count show up before
-- a polymorphic state with lower allele count, this would move some polymorphic
-- states closer to their respective boundaries),
instance Ord State where
  Bnd {} <= Ply {}            = True
  Ply {} <= Bnd {}            = False
  s@(Bnd n a) <= t@(Bnd m b)
    | s == t                  = True
    | n /= m                  = n <= m
    | otherwise               = a <= b
  s@(Ply n i a b) <= t@(Ply m j c d)
    | s == t                  = True
    | n /= m                  = n <= m
    | a < c                   = True
    | a > c                   = False
    -- We can be sure that a  = c now.
    | b < d                   = True
    | b > d                   = False
    -- Now we can be sure that both nucleotides are the same.
    | otherwise               = i <= j

-- | Fixed population size when converting a 'State' to or from a number. In
-- this case, a fixed population size is necessary so that @toEnum . fromEnum ==
-- id@. When converting from a number to 'State', the population size has to be
-- given or assumed (see 'fromIndexWith') anyways. Especially when performing IO,
-- the same number should always correspond to the same 'State' (bijection).
-- 'nFixed' has been set such that the size of the state space is 256.
nFixed :: Int
nFixed = 43

-- | Set the population size of a 'State'; validity of resulting 'State' is checked.
setPopSize :: PopSize -> State -> Maybe State
setPopSize n s = if valid s' then Just s' else Nothing
  where s' = unsafeSetPopSize n s

-- | See 'setPopSize'. Does not check if resulting 'State' is valid.
unsafeSetPopSize :: Int -> State -> State
unsafeSetPopSize n (Bnd _ s)     = Bnd n s
unsafeSetPopSize n (Ply _ i a b) = Ply n i a b

-- | For a given population size 'PopSize', convert a number 'Int' to 'State'.
fromIndexWith :: PopSize -> Int -> State
fromIndexWith n i
  | i >= stateSpaceSize n = error $
    "Index " ++ show i ++ "out of bounds when population size is " ++ show n ++ "."
  | i < nAlleles = Bnd n (toEnum i)
  | otherwise = Ply n (i' - p^._1 + 1) (p^._2) (p^._3)
  where i' = i - nAlleles
        l = [ (enumCombination a b * (n-1), a, b)
            | a <- [minBound .. pred maxBound]
            , b <- [ succ a ..]]
        p = last $ takeWhile (\e -> e^._1 <= i') l

-- | Convert 'State' to a number 'Int' for the given population size 'PopSize'.
-- Back conversion can be done with 'fromIndexWith', with the same population size.
toIndex :: State -> Int
toIndex (Bnd _ a) = fromEnum a
-- We also have to shift the enumeration value by the number of boundary
-- states, which is 'nAlleles'.
toIndex (Ply n i a b) = nAlleles + enumCombination a b * (n-1) + i-1

-- | Enumeration only works when the population size is 'nFixed'. Only then,
-- @toEnum . fromEnum == id@ can be guaranteed. This is because @toEnum ::
-- State@ is only defined if the population size is known. See also
-- 'fromIndexWith', and 'toIndex', as well as, 'setPopSize'.
instance Enum State where
  fromEnum s = if getPopSize s /= nFixed
    then error $ "State is not enumerable: " ++ show s ++ "."
    else toIndex s
  toEnum = fromIndexWith nFixed

-- The formula is a little complicated. Sketch of derivation: Order the states
-- in the following way:
-- @
--  AC CG GT
--  AG CT
--  AT
-- @
-- The edge length of the triangle is @'nAlleles' - 1@. Use Gauss's triangle
-- equation @area=binom(length+1, 2)@ twice to count the number of combinations
-- up to a certain allele. E.g., up to, but excluding G:
-- @
--  AC CG
--  AG CT
--  AT
-- @
countCombinationsUpToAllele :: Allele -> Int
countCombinationsUpToAllele a = round $ nAlleles `choose` 2 - (nAlleles - fromEnum a) `choose` 2

-- See 'countCombinationsUpToAllele'. The @-1@ pops up because we start counting
-- from 0. For example, the enumeration value of @GT@ (with @fromEnum G = k = 2@
-- and @fromEnum T = 3@) is then @enumCombinationsUpToK 2 + (3-2)@.
enumCombination :: Allele -> Allele -> Int
enumCombination a b = countCombinationsUpToAllele a - 1 + (fromEnum b - fromEnum a)

-- | A fixed population size 'nFixed' is assumed.
instance Bounded State where
  minBound = Bnd nFixed minBound
  maxBound = Ply nFixed (nFixed-1) (pred maxBound) maxBound

-- XXX: I am not sure if I should instantiate 'Character' because writing Fasta
-- files with boundary mutation model states is not really promising anyways.
-- However, the 'toIndex' and 'fromIndexWith' function provide a convenient way to
-- map states to integers. This functionality is needed when working with
-- matrices.
-- | A fixed population size 'nFixed' is assumed.
instance Character State where
  fromWord = toEnum . fromEnum
  toWord = toEnum . fromEnum

valid :: State -> Bool
valid (Bnd n _)
  | n <= 1    = False
  | otherwise = True
valid (Ply n i a b)
  | n <= 1    = False
  | a >= b    = False
  | i <= 0    = False
  | i >= n    = False
  | otherwise = True

filterValidStates :: [State] -> [State]
filterValidStates = filter valid

getPopSize :: State -> PopSize
getPopSize (Bnd n _)     = n
getPopSize (Ply n _ _ _) = n

-- CCC: This function is a not very efficient. A better would be something like:
-- @
-- | Sorted list of all possible PoMo states for a specific population size.
stateSpace :: PopSize -> [State]
stateSpace n = map (fromIndexWith n) [0 .. stateSpaceSize n - 1]
-- stateSpace n
--   | n <= 1    = error "The population size has to be larger than one."
--   | otherwise = sort $ filterValidStates ( allBndStates ++ allPlyStates )
--   where allBndStates = [ Bnd n a |
--                          a <- [minBound .. maxBound] :: [Allele] ]
--         allPlyStates = [ Ply n i a b |
--                          i <- [0..n],
--                          a <- [minBound .. maxBound] :: [Allele],
--                          b <- [minBound .. maxBound] :: [Allele] ]

-- | The state space of the boundary mutation model for four alleles and a
-- population size N is 4 + 6*(N-1).
stateSpaceSize :: PopSize -> Int
stateSpaceSize n = k + k*(k-1) `div` 2 * (n-1)
  where k = nAlleles


-- -- This is a very convenient version of toIndex, but can we always guarantee
-- -- that the state space is sorted the same way?
-- -- | Convert a boundary state to its ID (integer). See also 'idToState'.
-- stateId :: State -> Maybe Int
-- stateId s = elemIndex s (stateSpace $ getPopSize s)

-- -- Same here.
-- -- | Convert an ID to a boundary state. See also 'stateID'.
-- idToState :: PopSize -> Int -> State
-- idToState n i = stateSpace n !! i

-- | Check if two states are connected. By definition, states are NOT connected
-- with themselves.
neighbors :: State -> State -> Bool
neighbors s t = s `elem` getNeighbors t

getNeighbors :: State -> [State]
getNeighbors (Bnd n a) = filterValidStates allNeighbors
  where allNeighbors = [ Ply n (n-1) a b |
                         b <- [minBound .. maxBound] :: [Allele] ]
                       ++
                       [ Ply n 1 b a |
                         b <- [minBound .. maxBound] :: [Allele] ]
getNeighbors (Ply n i a b)
  -- Careful when the population size is two, because then each polymorphic
  -- states has two boundary states as neighbors.
  | i == 1 && n == 2  = Bnd n a : [Bnd n b]
  | i == 1            = Bnd n b : [Ply n 2 a b]
  | i == (n-1)        = Bnd n a : [Ply n (n-2) a b]
  | otherwise         = Ply n (i+1) a b : [Ply n (i-1) a b]
