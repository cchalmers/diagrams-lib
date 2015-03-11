{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Lens
import Numeric.Lens
import Data.Typeable
import GHC.TypeLits
import Data.Data

class (Show a, Eq a, Ord a, Typeable a, Data a) => Constrains a
instance (Show a, Eq a, Ord a, Typeable a, Data a) => Constrains a

class (f a, g a) => (f :*: g) a
instance (f a, g a) => (f :*: g) a

type S = Show
type O = Ord
type E = Eq

newtype A = A Int deriving (S,O,E)

-- import Data.Reflection

-- |
-- Reflection interface for specifying DPI. Users can either specify
-- a orphan @Given DPI@ instance or use 'withDpi' to specify which
-- 'DPI' to use.

-- | Wrapper to prevent using the wrong type when converting units.
--   The intuition is the opposite of 'Angle'. The 'Unit' type
--   represents some arbitrary unit (just like @x@ in @x \@\@ rad@) and
--   the extracted type is always in pixels.
--
--   For 'Angle' we had
--
-- @
-- rad :: Iso' (Angle n) n
-- @
--
--   but now we have
--
-- @
-- px :: Iso' n (Unit n)
-- @
--
newtype Unit n = Unit n
  deriving (Show, Read, Eq, Ord, Num, Fractional, Real, RealFrac, Typeable)

px :: (Eq n, Fractional n) => Iso' n (Unit n)
px = iso Unit (\(Unit n) -> n)

px' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
px' = const px

inch' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
inch' dpi = dividing dpi . px

inch :: (Eq n, Fractional n) => Iso' n (Unit n)
inch = inch' 96

m' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
m' dpi = inch' (dpi / 0.254)

m :: (Eq n, Fractional n) => Iso' n (Unit n)
m = m' 96

mm' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
mm' dpi = inch' (dpi / 25.4)

mm :: (Eq n, Fractional n) => Iso' n (Unit n)
mm = mm' 96

cm' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
cm' dpi = inch' (dpi / 2.54)

cm :: (Eq n, Fractional n) => Iso' n (Unit n)
cm = cm' 96

pt' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
pt' dpi = inch' (dpi / 72.27)

pt :: (Eq n, Fractional n) => Iso' n (Unit n)
pt = pt' 96

pc' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
pc' dpi = inch' (dpi / 6)

pc :: (Eq n, Fractional n) => Iso' n (Unit n)
pc = pc' 96

bp' :: (Eq n, Fractional n) => n -> Iso' n (Unit n)
bp' dpi = inch' (dpi / 72)

bp :: (Eq n, Fractional n) => Iso' n (Unit n)
bp = bp' 96

newtype Distance (l :: Symbol) n = D n
  deriving (Eq, Ord, Num, Fractional)

instance (Show n, KnownSymbol l) => Show (Distance l n) where
  show (D n) = take 6 (show n) ++ " " ++ symbolVal (Proxy :: Proxy l)


class Unit_ (l :: Symbol) where
  convertMulti :: Fractional n => Proxy l -> n -> n

instance Unit_ "m"  where convertMulti _ dpi = dpi/0.254
instance Unit_ "cm" where convertMulti _ dpi = dpi/2.54
instance Unit_ "mm" where convertMulti _ dpi = dpi/25.4
instance Unit_ "bp" where convertMulti _ dpi = dpi/96
instance Unit_ "px" where convertMulti _ _   = 1

-- | Extract a unit back to diagram's native px units.
extractUnit :: (Unit_ l, Eq n, Fractional n) => Distance l n -> n
extractUnit = review unit_

unit'_ :: forall l n. (Eq n, Fractional n, Unit_ l)
       => n -> Iso' n (Distance l n)
unit'_ dpi = dividing (convertMulti (Proxy :: Proxy l) dpi)
           . iso D (\(D n) -> n)

unit_ :: forall l n. (Eq n, Fractional n, Unit_ l)
       => Iso' n (Distance l n)
unit_ = unit'_ 96

convert' :: (Eq n, Fractional n, Unit_ l1, Unit_ l2)
         => n -> Distance l1 n -> Distance l2 n
convert' dpi = view (unit'_ dpi) . review (unit'_ dpi)

convert :: (Eq n, Fractional n, Unit_ l1, Unit_ l2)
        => Distance l1 n -> Distance l2 n
convert = convert' 96

cm'_ :: (Eq n, Fractional n) => n -> Iso' n (Distance "cm" n)
cm'_ = unit'_

cm_ :: (Eq n, Fractional n) => Iso' n (Distance "cm" n)
cm_ = unit_

px_ :: (Eq n, Fractional n) => Iso' n (Distance "px" n)
px_ = unit_

data KnownUnit where
  KnownUnit :: (KnownSymbol l, Unit_ l) => Proxy l -> KnownUnit

knownUnits :: [KnownUnit]
knownUnits =
  [ KnownUnit (Proxy :: Proxy "px")
  , KnownUnit (Proxy :: Proxy "cm")
  ]

searchUnit :: (KnownSymbol a, Fractional n)
           => n -> Proxy a -> [KnownUnit] -> Maybe n
searchUnit dpi p knowns = go knowns
  where
    go (KnownUnit q:xs) =
      case sameSymbol p q of
        Nothing   -> go xs
        Just Refl -> Just $ convertMulti p dpi
    go [] = Nothing

-- overUnit :: Unit_ l => String -> Distance "px" Double
--          -> (Double -> Double) -> Maybe (Distance "px" Double)
-- overUnit l d f = case someSymbolVal l of
--   where
--     s = someSymbolVal l

-- newtype DPI = DPI Rational
--   deriving (Show, Read, Eq, Ord, Num, Fractional, Real, RealFrac)

-- fromDpi :: Fractional n => DPI -> n
-- fromDpi (DPI n) = fromRational n

-- dpi :: (Given DPI, Fractional n) => n
-- dpi = fromDpi given

-- withDpi :: DPI -> (Given DPI => a) -> a
-- withDpi = give

-- -- #define REFLECT reflect (Proxy :: Proxy s)

-- cm :: (Given DPI, Eq n, Fractional n) => Iso' n n
-- cm = multiplying (dpi / 2.54)
