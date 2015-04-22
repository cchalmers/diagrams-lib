-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Points
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Points in space.  For more tools for working with points and
-- vectors, see "Linear.Affine".
--
-----------------------------------------------------------------------------

module Diagrams.Points
  ( -- * Points
    Point (..)
   , origin
   , (*.)

    -- * Point-related utilities
  , centroid
  -- , pointDiagram
  , _Point
  , mirror
  , relative
  , relative2
  , relative3
  , reflectThrough
  ) where

-- import           Diagrams.Core        (pointDiagram)
-- import           Diagrams.Core.Points

import           Data.Foldable        as F
import Control.Lens

import           Linear.Affine
import           Linear.Vector

-- | The centroid of a set of /n/ points is their sum divided by /n/.
centroid :: (Additive v, Fractional n) => [Point v n] -> Point v n
centroid = meanV

meanV :: (Foldable f, Additive v, Fractional a) => f (v a) -> v a
meanV = uncurry (^/) . F.foldl' (\(s,c) e -> (e ^+^ s,c+1)) (zero,0)
{-# INLINE meanV #-}

mirror :: (Additive v, Num n) => Point v n -> Point v n
mirror = reflectThrough origin

-- | Scale a point by a scalar. Specialized version of '(*^)'.
(*.) :: (Functor v, Num n) => n -> Point v n -> Point v n
(*.) = (*^)

-- | Apply a transformation relative to the given point.
relative2 :: (Additive v, Num n)
  => Point v n -> (v n -> v n -> v n)
  -> Point v n -> Point v n -> Point v n
relative2 p f x y = (p .+^) $ f (inj x) (inj y) where inj = (.-. p)

-- | Apply a transformation relative to the given point.
relative3 :: (Additive v, Num n)
  => Point v n -> (v n -> v n -> v n -> v n)
  -> Point v n -> Point v n -> Point v n -> Point v n
relative3 p f x y z = (p .+^) $ f (inj x) (inj y) (inj z) where inj = (.-. p)

-- | Mirror a point through a given point.
reflectThrough :: (Additive v, Num n) => Point v n -> Point v n -> Point v n
reflectThrough o = over (relative o) negated

