{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Diagrams.ThreeD.Projections where

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Projections
-- Copyright   :  (c) 2014
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- 3D projections are a way of viewing a three-dimensional objects on a
-- two-dimensional plane.
--
--
-----------------------------------------------------------------------------

import           Control.Lens              hiding (transform)
import Data.Typeable
import Data.Foldable
import Control.Applicative

import Diagrams.Core
import Linear
import           Data.Semigroup
import           Diagrams.Angle
-- import           Diagrams.Core.HasOrigin
import           Diagrams.ThreeD.Camera
import           Diagrams.ThreeD.Transform
-- import           Diagrams.ThreeD.Types
import           Diagrams.TwoD             hiding (view)
-- -- import Diagrams.Core
import           Diagrams.Direction
import           Diagrams.LinearMap


newtype Euler n = Euler (V3 n)
  deriving (Show, Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable, Typeable)

makeWrapped ''Euler

type instance V (Euler n) = V3
type instance N (Euler n) = n

_yaw :: Lens' (Euler n) (Angle n)
_yaw = _Wrapped' . _x . from rad

_pitch :: Lens' (Euler n) (Angle n)
_pitch = _Wrapped' . _y . from rad

_roll :: Lens' (Euler n) (Angle n)
_roll = _Wrapped' . _z . from rad

-- eulerLinearMap :: Euler n -> LinearMap n

quatToEuler :: RealFloat n => Quaternion n -> Euler n
quatToEuler (Quaternion q0 (V3 q1 q2 q3)) = Euler $ V3 yaw pitch roll
  where
    r11 = q0*q0 + q1*q1 - q2*q2 - q3*q3
    r12 = 2.0*(q1*q2 + q0*q3)
    mr13' = -2.0*(q1*q3 - q0*q2)
    mr13 -- nan protect
      | mr13' >  1 =  1
      | mr13' < -1 = -1
      | otherwise = mr13'
    r23 = 2.0*(q2*q3 + q0*q1)
    r33 = q0*q0 - q1*q1 - q2*q2 + q3*q3

    yaw   = atan2 r12 r11
    pitch = asin mr13
    roll  = atan2 r23 r33

eulerToQuat :: (Floating n, Ord n) => Euler n -> Quaternion n
eulerToQuat (Euler (V3 yaw pitch roll)) = signorm q
  where
    sr2 = sin $ 0.5 * roll
    cr2 = cos $ 0.5 * roll
    sp2 = sin $ 0.5 * pitch
    cp2 = cos $ 0.5 * pitch
    sy2 = sin $ 0.5 * yaw
    cy2 = cos $ 0.5 * yaw
    q0 = cr2*cp2*cy2 + sr2*sp2*sy2
    q1 = sr2*cp2*cy2 - cr2*sp2*sy2
    q2 = cr2*sp2*cy2 + sr2*cp2*sy2
    q3 = cr2*cp2*sy2 - sr2*sp2*cy2

    q' = Quaternion q0 (V3 q1 q2 q3)

    q
      | q0 < 0 = Quaternion (-q0) (V3 (-q1) (-q2) (-q3))
      | otherwise = q'

eulerToM33 :: (Floating a, Ord a) => Euler a -> M33 a
eulerToM33 = fromQuaternion . eulerToQuat

m33ToEuler :: RealFloat a => M33 a -> Euler a
m33ToEuler
  (V3
   (V3 r11 r12 r13)
   (V3   _   _ r23)
   (V3   _   _ r33)) = Euler (V3 yaw pitch roll)
  where
    mr13' = -r13
    mr13 -- nan protect
      | mr13' >  1 =  1
      | mr13' < -1 = -1
      | otherwise = mr13'

    yaw   = atan2 r12 r11
    pitch = asin mr13
    roll  = atan2 r23 r33


