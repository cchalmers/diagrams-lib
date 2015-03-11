{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Ellipse
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional ellipses (and, as a special case, circles).
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Ellipse
    (
      -- * Ellipse and circle diagrams
      unitCircle
    , circle
    , ellipse
    , ellipseXY
    ) where

import           Diagrams.Core

import           Diagrams.Angle
import           Diagrams.Located        (at)
import           Diagrams.Trail          (glueTrail)
import           Diagrams.TrailLike
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Transform
import           Diagrams.TwoD.Types
import           Diagrams.TwoD.Vector    (xDir)
import           Diagrams.Util

-- | A circle of radius 1, with center at the origin.
unitCircle :: (InSpace V2 n t, Floating n, TrailLike t) => t
unitCircle = trailLike $ glueTrail (arcT xDir fullTurn) `at` mkP2 1 0
-- unitCircle =
  where
    circle' = fromSegments
                [ cubic (V2 0 a)    (V2 (-b) 1)    (V2 (-1) 1)
                , cubic (V2 (-a) 0) (V2 (-1) (-b)) (V2 (-1) (-1))
                , cubic (V2 0 (-1)) (V2 b (-1))    (V2 1 (-1))
                , cubic (V2 0 (-a)) (
                ]
             -- cubic (V2 0 (-a))
    a = 0.75 * (sqrt 2 - 1)
    b = 0.75 * (1 - sqrt 2)

-- fromList [
--   Cubic (V2 0.0 0.5522847) (V2 (-0.44771528) 1.0) (OffsetClosed (V2 (-1.0) 0.99999994)),
--   Cubic (V2 (-0.5522847) 0) (V2 (-1) (-0.4477153)) (OffsetClosed (V2 (-1) (-1))),
--   Cubic (V2 0 (-0.5522847)) (V2 0.44771534 (-1)) (OffsetClosed (V2 1.0 (-1))),
-- ]
-- ))
-- (Cubic (V2 0.5522847 7.24234e-8) (V2 0.9999999 0.44771525) OffsetOpen))}

-- | A circle of the given radius, centered at the origin.  As a path,
--   it begins at (r,0).
circle :: (InSpace V2 n t, Floating n, TrailLike t) => n -> t
circle d = trailLike $ unitCircle # scale d

-- | @ellipse e@ constructs an ellipse with eccentricity @e@ by
--   scaling the unit circle in the x-direction.  The eccentricity must
--   be within the interval [0,1).
ellipse :: (InSpace V2 n t, Floating n, TrailLike t) => n -> t
ellipse e
  | e >= 0 && e < 1 = trailLike $ scaleX (sqrt (1 - e*e)) unitCircle
  | otherwise       = error "Eccentricity of ellipse must be >= 0 and < 1."

-- | @ellipseXY x y@ creates an axis-aligned ellipse, centered at the
--   origin, with radius @x@ along the x-axis and radius @y@ along the
--   y-axis.
ellipseXY :: (InSpace V2 n t, Floating n, TrailLike t) => n -> n -> t
ellipseXY x y = trailLike $ unitCircle # scaleX x # scaleY y
