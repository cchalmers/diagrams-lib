{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Diagrams.ThreeD.Projections where

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.ThreeD.Projections
-- Copyright   :  (c) 2014 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- 3D projections are a way of viewing a three-dimensional objects on a
-- two-dimensional plane.
--
--
-----------------------------------------------------------------------------

import           Control.Applicative
import           Control.Lens              hiding (transform)
import           Data.Foldable
import           Data.Typeable

import           Data.Semigroup
import           Diagrams.Angle
import           Diagrams.Core
import           Linear
-- import           Diagrams.Core.HasOrigin
import           Diagrams.ThreeD.Camera
import           Diagrams.ThreeD.Transform
-- import           Diagrams.ThreeD.Types
import           Diagrams.TwoD             hiding (view)
-- -- import Diagrams.Core
import           Diagrams.Direction
import           Diagrams.LinearMap

-- * Parallel projections

-- ** Orthographic projections
-- Orthographic projections are a form of parallel projections where are
-- projection lines are orthogonal to the projection plane.
--

-- ** Axonometric projection
-- Axonometric projections are a type of orthographic projection where the
-- plane of the object is not parallel to the projection plane. This is the
-- most common projection for technical drawings and plots.

-- *** Common axonometric projections

-- -- | Affine map for viewing thought a othogonal camera lens. Ignores width and
-- --   depth parameters.
-- orthoCameraProjection :: Floating n => Camera OrthoLens n -> AffineMap V3 V2 n
-- orthoCameraProjection cam
--   = transformProject (moveOriginTo (camLoc cam) mempty <> rot)
--   where
--     rot = pointAt (camForward cam) xDir (camUp cam)

transformProject :: Floating n => T3 n -> AffineMap V3 V2 n
transformProject t = AffineMap (LinearMap $ view _xy . apply t) v
  where
    linearMap = view _xy . apply t

-- rotateTo :: Floating n => Direction V3 n -> T3 n
-- rotateTo = rotateToX . fromDirection
--
-- rotateToX :: Floating n => V3 n -> T3 n
-- rotateToX v = pointAt' v unitX (V3 0 0 1)
--
-- orthoProjectDir :: Floating n => Direction V3 n -> AffineMap V3 V2 n
-- orthoProjectDir = transProject . rotateTo

isometricTransform :: Floating n => T3 n
isometricTransform = aboutY (45@@deg) <> aboutZ (asinA (tan (pi/6)))

isometricProjection :: Floating n => AffineMap V3 V2 n
isometricProjection = transformProject isometricTransform

