-- |
-- Module      :  Phonetic.Languages.Permutations
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Commonly used versions of the @phonetic-languages-common@ package functions.

module Phonetic.Languages.Permutations (
  universalSetG
  , universalSetGL
  , genPermutations
  , genPermutationsV
  , genPermutationsL
  , genPermutationsVL
) where

import qualified Data.Vector as VB
import qualified Data.List as L (permutations)
import Data.SubG
import Data.SubG.InstancesPlus ()
import qualified Data.Foldable as F (concat,foldr')
--import Data.List ()
import Data.Monoid

-- | A key point of the evaluation -- the universal set of the task represented as a 'VB.Vector' of 'VB.Vector' of @a@.
universalSetG ::
  (Eq a, Foldable t, InsertLeft t a, Monoid (t a), Monoid (t (t a))) => t a
  -> t (t a)
  -> (t a -> VB.Vector a) -- ^ The function that is used internally to convert to the boxed 'VB.Vector' of @a@ so that the function can process further the permutations
  -> ((t (t a)) -> VB.Vector (VB.Vector a)) -- ^ The function that is used internally to convert to the boxed 'VB.Vector' of 'VB.Vector' of @a@ so that the function can process further
  -> VB.Vector (VB.Vector Int) -- ^ The list of permutations of 'Int' indices starting from 0 and up to n (n is probably less than 7).
  -> VB.Vector (VB.Vector a)
  -> VB.Vector (VB.Vector a)
universalSetG ts uss f1 f2 perms baseV = VB.map (VB.foldr' mappend mempty . VB.cons (f1 ts) . (`mappend` (f2 uss)) . VB.unsafeBackpermute baseV) perms
{-# INLINE universalSetG #-}

-- | A key point of the evaluation -- the universal set of the task represented as a 'VB.Vector' of 'VB.Vector' of @a@. Because the order is not
universalSetGL ::
  (Eq a, Foldable t, InsertLeft t a, Monoid (t a), Monoid (t (t a))) => t a
  -> t (t a)
  -> (t a -> [a]) -- ^ The function that is used internally to convert to the @[a]@ so that the function can process further the permutations
  -> ((t (t a)) -> [[a]]) -- ^ The function that is used internally to convert to the needed representation so that the function can process further
  -> [VB.Vector Int] -- ^ The list of permutations of 'Int' indices starting from 0 and up to n (n is probably less than 7).
  -> VB.Vector [a]
  -> [[a]]
universalSetGL ts uss f1 f2 permsL baseV = map (F.concat . F.foldr' (:) [] . (f1 ts:) . (`mappend` f2 uss) . VB.toList . VB.unsafeBackpermute baseV) permsL
{-# INLINE universalSetGL #-}

genPermutations :: Int -> VB.Vector (VB.Vector Int)
genPermutations n = VB.map VB.fromList . VB.fromList . L.permutations . take n $ [0..]
{-# INLINE genPermutations #-}

genPermutationsV :: VB.Vector (VB.Vector (VB.Vector Int))
genPermutationsV = VB.map (\n -> VB.map VB.fromList . VB.fromList . L.permutations . take n $ [0..]) . VB.enumFromTo 2 $ 7
{-# INLINE genPermutationsV #-}

genPermutationsL :: Int -> [VB.Vector Int]
genPermutationsL n = map VB.fromList . L.permutations . take n $ [0..]
{-# INLINE genPermutationsL #-}

genPermutationsVL :: VB.Vector [VB.Vector Int]
genPermutationsVL = VB.map (\n -> map VB.fromList . L.permutations . take n $ [0..]) . VB.enumFromTo 2 $ 7
{-# INLINE genPermutationsVL #-}
