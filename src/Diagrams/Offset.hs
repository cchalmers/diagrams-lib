{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Diagrams.Offset where

import Diagrams.Segment
import Diagrams.Trail hiding (offset)
import Control.Lens hiding (at, transform)
import Diagrams.Core
import Control.Applicative
import Diagrams.Located
import Diagrams.Path
import Data.Monoid hiding ((<>))
import           Data.FingerTree     (traverse')

import Data.Traversable

import Linear
import Linear.Affine
-- import Data.Functor.Apply

import Diagrams.TwoD.Types (P2)

-- import Control.Applicative
-- import Control.Arrow as Arrow
import Control.Category
-- import Control.Comonad
import Control.Lens.Internal.Instances ()
-- import Control.Monad
-- import Control.Monad.Fix
-- import Data.Distributive
-- import Data.Functor.Bind
-- import Data.Functor.Contravariant
-- import Data.Int
-- import Data.Profunctor
-- import Data.Profunctor.Rep
-- import Data.Traversable
import Prelude hiding ((.),id)
-- import Control.Monad.State.Lazy
-- import Data.Tuple (swap)

-- import Data.Profunctor.Rep
-- import Control.Comonad
import Data.Typeable
-- import Diagrams.TwoD hiding (p2)

import Diagrams.Core.Compile
-- import Diagrams.Core.Types
import           Data.Tree                    (Tree (Node))
import Data.Foldable (foldMap)


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

asPath :: Path v n -> Path v n
asPath = id

asTrail :: Trail v n -> Trail v n
asTrail = id

asLine :: Trail' Line v n -> Trail' Line v n
asLine = id

asLoop :: Trail' Loop v n -> Trail' Loop v n
asLoop = id

asLocTrail :: Located (Trail v n) -> Located (Trail v n)
asLocTrail = id

asLocLine :: Located (Trail' Line v n) -> Located (Trail' Line v n)
asLocLine = id

asLocLoop :: Located (Trail' Loop v n) -> Located (Trail' Loop v n)
asLocLoop = id

asOffsets :: [v n] -> [v n]
asOffsets = id

asVertices :: [Point v n] -> [Point v n]
asVertices = id

asDiagram :: b -> Diagram b -> Diagram b
asDiagram _ = id

asADiagram :: QDiagram b v n Any -> QDiagram b v n Any
asADiagram = id

asQDiagram :: QDiagram b v n m -> QDiagram b v n m
asQDiagram = id

forBackend :: SameSpace b t => b -> t -> t
forBackend _ = id

usingDouble :: N t ~ Double => t -> t
usingDouble = id

usingFloat :: N t ~ Float => t -> t
usingFloat = id

inV2 :: V t ~ V2 => t -> t
inV2 = id

inV3 :: V t ~ V3 => t -> t
inV3 = id

------------------------------------------------------------------------
-- Offsets
------------------------------------------------------------------------

class HasOffsets t where
  offsets :: InSpace v n t => Traversal' t (v n)

  totalOffset :: InSpace v n t => t -> v n

  default totalOffset :: (InSpace v n t, Additive v, Num n) => t -> v n
  totalOffset = sumV . toListOf offsets

instance HasOffsets (Offset c v n) where
  offsets = each

  totalOffset (OffsetClosed v) = v
  totalOffset OffsetOpen       = zero

instance HasOffsets (Segment c v n) where
  offsets f (Linear off)      = Linear <$> offsets f off
  offsets f (Cubic v1 v2 off) = Cubic  <$> pure v1 <*> pure v2 <*> offsets f off

  totalOffset (Linear off)    = totalOffset off
  totalOffset (Cubic _ _ off) = totalOffset off

fixedSegment :: (Additive v, Additive v', Num n, Num n')
             => Iso (Located (Segment Closed v n)) (Located (Segment Closed v' n'))
                    (FixedSegment v n)             (FixedSegment v' n')
fixedSegment = iso mkFixedSeg fromFixedSeg

instance HasOffsets t => HasOffsets (Located t) where
  offsets = located . offsets

  totalOffset = totalOffset . unLoc

instance (Additive v, Num n) => HasOffsets (FixedSegment v n) where
  offsets = from fixedSegment . offsets

  totalOffset (FLinear p0 p1)    = p1 .-. p0
  totalOffset (FCubic p0 _ _ p3) = p3 .-. p0

instance (Metric v, OrderedField n) => HasOffsets (SegTree v n) where
  offsets = _Wrapped . traverse' . offsets

  totalOffset = totalOffset . Line

instance (Metric v, OrderedField n) => HasOffsets (Trail' l v n) where
  offsets f (Line st)     = Line <$> offsets f st
  offsets f (Loop st off) = Loop <$> offsets f st <*> offsets f off

  totalOffset l@(Line _) = lineOffset l
  totalOffset _          = zero

instance (Metric v, OrderedField n) => HasOffsets (Trail v n) where
  offsets f (Trail t) = Trail <$> offsets f t

  totalOffset (Trail t) = totalOffset t

instance HasOffsets [V2 Double] where
  offsets = traverse

instance HasOffsets [Point V2 Double] where
  offsets = pVs

------------------------------------------------------------------------
-- Segments
------------------------------------------------------------------------

class HasSegments t where
  closedSegments :: InSpace v n t => Traversal' t (Segment Closed v n)

  openSegments :: InSpace v n t => Traversal' t (Segment Open v n)

instance HasSegments (Segment c v n) where
  closedSegments f a@(Linear (OffsetClosed _))    = f a
  closedSegments f a@(Cubic _ _ (OffsetClosed _)) = f a
  closedSegments _ a                              = pure a

  openSegments f a@(Linear OffsetOpen)    = f a
  openSegments f a@(Cubic _ _ OffsetOpen) = f a
  openSegments _ a                        = pure a

instance HasSegments t => HasSegments (Located t) where
  openSegments   = located . openSegments
  closedSegments = located . closedSegments

instance (Additive v, Num n) => HasSegments (FixedSegment v n) where
  closedSegments = from fixedSegment . closedSegments
  openSegments   = ignored

instance (Metric v, OrderedField n) => HasSegments (SegTree v n) where
  closedSegments = _Wrapped . traverse' . closedSegments
  openSegments   = ignored

instance (Metric v, OrderedField n) => HasSegments (Trail' l v n) where
  closedSegments f (Line st)     = Line <$> closedSegments f st
  closedSegments f (Loop st off) = Loop <$> closedSegments f st <*> pure off

  openSegments f (Loop st off) = Loop <$> pure st <*> openSegments f off
  openSegments f (Line st)     = Line <$> openSegments f st

instance (Metric v, OrderedField n) => HasSegments (Trail v n) where
  closedSegments f (Trail t) = Trail <$> closedSegments f t
  openSegments f (Trail t)   = Trail <$> openSegments f t

offsets_ :: (InSpace v n t, HasSegments t) => Traversal' t (v n)
offsets_ = closedSegments . offsets

controls :: (InSpace v n t, HasSegments t) => Traversal' t (v n)
controls = closedSegments . each

------------------------------------------------------------------------
-- Reverse
------------------------------------------------------------------------

instance (Additive v, Num n) => Reversing (Offset Closed v n) where
  reversing (OffsetClosed off) = OffsetClosed $ negated off

instance (Additive v, Num n) => Reversing (Segment Closed v n) where
  reversing (Linear off)      = Linear $ reversing off
  reversing (Cubic v1 v2 off) = Cubic (v2 ^-^ v3) (v1 ^-^ v3) (reversing off)
    where v3 = totalOffset off

instance Reversing (FixedSegment v n) where
  reversing (FLinear p0 p1)      = FLinear p1 p0
  reversing (FCubic p0 p1 p2 p3) = FCubic p3 p2 p1 p0

instance (Metric v, OrderedField n) => Reversing (Trail' l v n) where
  reversing t@(Line _)   = onLineSegments (reverse . map reversing) t
  reversing t@(Loop _ _) = glueLine . reversing . cutLoop $ t

instance (Metric v, OrderedField n) => Reversing (Trail v n) where
  reversing (Trail t) = Trail (reversing t)

instance (HasOffsets t, Reversing t, Additive (V t), Num (N t)) => Reversing (Located t) where
  reversing (viewLoc -> (p, t)) = reversing t `at` (p .+^ totalOffset t)

instance (Metric v, OrderedField n) => Reversing (Path v n) where
  reversing = _Wrapped' . mapped %~ reversing

------------------------------------------------------------------------
-- Verticies
------------------------------------------------------------------------

-- class HasPoints t where
--   points :: InSpace v n t => Fold t (Point v n)

-- it's too hard (inpossible?) to keep traversal laws
class HasPoints t where
  points :: InSpace v n t => Traversal' t (Point v n)

instance HasPoints (Point v n) where
  points = id

instance (Additive v, Num n) => HasPoints (FixedSegment v n) where
  points f (FLinear p0 p1)      = FLinear <$> f p0 <*> f p1
  points f (FCubic p0 p1 p2 p3) = FCubic  <$> f p0 <*> pure p1 <*> pure p2 <*> f p3


-- I want this but I can't get it to work:

instance (InSpace V2 Double t, HasOffsets t) => HasPoints (Located t) where
  points = diffs . traverse
    where
      diffs f (Loc p a) = f (accPoints p (a ^.. offsets)) <&>
        \ps@(p:_) -> Loc p (a & partsOf offsets .~ snd (diffPoints ps))

locPs :: Traversal' (Located [V2 Double]) (P2 Double)
locPs = vDiffs . traverse
  where
    vDiffs f (viewLoc -> (p, vs)) = f (accPoints p vs) <&> (uncurry Loc) . diffPoints

_Loc :: Iso' (Located a) (Point (V a) (N a), a)
_Loc = iso viewLoc (uncurry Loc)



-- this is just bad
traverseV :: Applicative f => P2 Double -> (P2 Double -> f (P2 Double)) -> [V2 Double] -> f [V2 Double]
-- traverseV p = traverse . from (relative p)
traverseV p f = foldrOf each cons_f (pure [])
  where cons_f x ys = (:) <$> from (relative p) f x <*> ys
-- traverseV p f v2 = mapAccumR cons_f p (pure []) vs
--   where cons_f x ys = (:) <$> (from (relative p) `asTypeOf` _) f x <*> ys

accPoints :: P2 Double -> [V2 Double] -> [P2 Double]
accPoints p0 vs = p0 : snd (mapAccumL acc_f p0 vs)
  where
    acc_f p v = (p .+^ v, p .+^ v)

diffPoints :: [P2 Double] -> (P2 Double, [V2 Double])
diffPoints (p0:ps0) = (p0, go p0 ps0)
  where go p1 (p2:ps) = (p2 .-. p1) : go p2 ps
        go _  []      = []
diffPoints _ = (origin, [])

pVs :: Traversal' [P2 Double] (V2 Double)
pVs = pDiffs . traverse
  where
    pDiffs f ps@(p:_) = f (snd $ diffPoints ps) <&> accPoints p
    pDiffs _ []       = pure []


vPs :: Traversal' (P2 Double, [V2 Double]) (P2 Double)
vPs = vDiffs . traverse
  where
    vDiffs f (p, vs) = f (accPoints p vs) <&> diffPoints

-- relativeTraverse :: P2 Double -> Traversal' (V2 Double) (P2 Double)
-- relativeTraverse p0 f a = traverse f (accPoints p0 a) <&> undefined

vss = [V2 5 5, V2 4 3, V2 10 10, V2 (-20) (-20)] :: [V2 Double]
pss = map (\(x,y) -> P (V2 x y)) [(1,2), (6,7), (10,10), (20, 20), (0,0)] :: [Point V2 Double]

class HasOffsets' t where
  offsets' :: Traversal' t (V2 Double)

-- mapAcumLOf :: Conjoined p => Over p (State acc) s t a b -> p acc (a -> (acc, b)) -> acc -> s -> t
-- mapAcumLOf l f acc0 s = evalState (l g s) acc0 where
--    g = cotabulate $ \wa -> state $ \acc -> swap (corep f (acc <$ wa) (extract wa))

-- mapAcumLOf' :: Conjoined p => Over p (State acc) s t a b -> p acc (a -> (acc, b)) -> acc -> s -> t
-- mapAcumLOf' l f acc0 s = evalState (l g s) acc0 where
--    g = cotabulate $ \wa -> state $ \acc -> swap (corep f (acc <$ wa) (extract wa))
-- This would be much cleaner if the argument order for the function was swapped.

-- pointsIso :: InSpace v n (v n) => Iso' (Located [v n]) [Point v n]
-- pointsIso = iso (\(viewLoc -> (p,vs)) -> p : vs ^.. accP p) unPoints

-- unPoints :: InSpace v n (v n) => [Point v n] -> Located [v n]
-- unPoints [] = [] `at` origin
-- unPoints (p0:ps) = go p0 ps `at` p0
--   where
--     go _ []     = []
    -- go p (a:as) = (a .-. p) : go a as


-- accP' :: (Applicative f, HasOffsets' t) => P2 Double -> (P2 Double -> f (P2 Double)) -> t -> f t
-- accP' p0 f vs = sequenceAOf offsets' . snd $ mapAccumLOf offsets' acc_f p0 vs
--   where
--     acc_f p v = (p .+^ v, from (relative p) f v)

-- tempAppend :: Point v n -> Iso' [v n] [v n]
-- tempAppend a = iso ([a ^. _Point] ++) tail

-- accP' :: (InSpace v n t, Applicative f, HasOffsets t) => Point v n -> (Point v n -> f (Point v n)) -> t -> f t
-- accP' p0 = partsOf offsets . tempAppend p0 . accP origin

-- accP :: (Additive v, Num n, Applicative f, Traversable t) => Point v n -> (Point v n -> f (Point v n)) -> t (v n) -> f (t (v n))
-- accP p0 f vs = sequenceA . snd $ mapAccumL acc_f p0 vs
--   where acc_f p v = let p' = p .+^ v in (p', from (relative p) f v)
  -- where acc_f p v = let p' = p .+^ v in (p', (\(P x) -> x) <$> f p')

-- acc_f
--   :: (Additive t, Num a, Functor f) =>
--      (Point t a -> f (Point t a))
--      -> Point t a -> t a -> (Point t a, f (t a))
-- acc_f f p v = (p .+^ v, from (relative p) f v)

-- accO :: Applicative f => P2 Double -> (P2 Double -> f (P2 Double)) -> [V2 Double] -> f [V2 Double]
-- accO p0 f vs = sequenceA . snd $ mapAccumLOf (offsets) acc_f p0 vs
--   where
--     acc_f p v = (p .+^ v, from (relative p) f v)

-- type Positioned = Indexed (P2 Double)

newtype Positioned f a = Positioned { runPositioned :: P2 Double -> (P2 Double, f a) }
instance Functor f => Functor (Positioned f) where
  fmap f (Positioned m) = Positioned $ \i -> case m i of
    (j, x) -> (j, fmap f x)

instance Applicative f => Applicative (Positioned f) where
  pure x = Positioned $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Positioned mf <*> Positioned ma = Positioned $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}


instance Contravariant f => Contravariant (Positioned f) where
  contramap f (Positioned m) = Positioned $ \p -> case m p of
    (j, ff) -> (j, contramap f ff)


-- @'indexing' :: 'Indexable' 'Int' p => 'Control.Lens.Type.LensLike' ('Indexing' f) s t a b -> 'Control.Lens.Type.Optical' p (->) f s t a b@
indexingP :: Indexable (P2 Double) p => P2 Double -> ((V2 Double -> Positioned f b) -> s -> Positioned f t) -> p (V2 Double) (f b) -> s -> f t
indexingP p0 l iafb s = snd $ runPositioned (l (\a -> Positioned (f a)) s) p0
-- indexingP p l iafb s = snd $ runPositioned (l (\a -> Positioned (\i -> i `seq` (i .+^ a, indexed iafb i a))) s) p
  where
    f a p = p' `seq` (p', indexed iafb p' a)
      where p' = p .+^ a

-- roundV :: Functor f => f Double -> f Double
-- roundV = fmap (fromInteger . round)

-- c :: [Point V2 Double]
-- c = [P (V2 1.0 0.0),P (V2 0.0 1.0),P (V2 (-1.0) 0.0),P (V2 0.0 (-1.0))]

-- d = c ^. from pointsIso

-- b = (circle 1 :: Located (Trail V2 Double) ) & offsets %~ roundV :: Located (Trail V2 Double)
-- c = b ^.. offsets


    -- Found hole ‘_’
    --   with type: (Point V2 Double -> f (Point V2 Double))
    --              -> V2 Double -> f (V2 Double)


 -- foldrOf :: Traversal' s a -> (a -> r -> r) -> r -> s -> r

 -- Traversable [] where
 --  {-# INLINE traverse #-} -- so that traverse can fuse
 --  traverse f = Prelude.foldr cons_f (pure [])
 --    where cons_f x ys = (:) <$> f x <*> ys

------------------------------------------------------------------------
-- Prisms
------------------------------------------------------------------------

_Line :: Prism' (Trail v n) (Trail' Line v n)
_Line = _Trail . _Left

_Loop :: Prism' (Trail v n) (Trail' Loop v n)
_Loop = _Trail . _Right

_Trail :: Iso (Trail v n)                              (Trail v' n')
          (Either (Trail' Line v n) (Trail' Loop v n)) (Either (Trail' Line v' n') (Trail' Loop v' n'))
_Trail = iso getTrail mkTrail
  where
    mkTrail (Right loop) = Trail loop
    mkTrail (Left line)  = Trail line

    getTrail :: Trail v n -> Either (Trail' Line v n) (Trail' Loop v n)
    getTrail (Trail t@(Line _))   = Left t
    getTrail (Trail t@(Loop _ _)) = Right t


------------------------------------------------------------------------
-- Diagram
------------------------------------------------------------------------

transformed :: (InSpace v n a, Transformable a) => Transformation v n -> Iso' a a
transformed t = iso (transform t) (transform $ inv t)

_Path :: (HasLinearMap v, Typeable v, Typeable n, Renderable (Path v n) b) => Prism' (Prim b v n) (Path v n)
_Path = prism' Prim (\(Prim a) -> cast a)

-- _RPrim :: Prism' (RNode b v n a) (Prim b v n)
-- _RPrim = prism' RPrim

makePrisms ''RNode

getPaths
  :: (Renderable (Path v n) b, HasLinearMap v, Metric v, OrderedField n, Monoid' m,
      Typeable n, Typeable v)
   => QDiagram b v n m -> [Path v n]
getPaths dia = toRTree mempty dia ^.. folded . _RPrim . _Path

-- toRender :: forall (OrderedField n, RealFloat n, Typeable n) => RTree b v n Annotation -> [Path v n]
toPaths
  :: (Renderable (Path v n) b, HasLinearMap v,
      Additive v, Traversable v, Typeable n, Typeable v)
   => Tree (RNode b v n m) -> [Path v n]
toPaths (Node (RPrim p) _) = p ^.. _Path
toPaths (Node _ rs)        = foldMap toPaths rs

-- s ^.. each . _TAttribute . to getFillTexture . _SC . to toAlphaColour

-- toPaths (Node (RStyle _) _) = []
-- toRender (Node (RStyle sty) rs) = R . P.scope $ do
--   oldSty <- P.style <<<>= sty
--   P.ignoreFill .= False
--
--   setClipPaths <~ op Clip
--   let R r = foldMap toRender rs
--   pgf <- r
--
--   P.style .= oldSty
--
--   return pgf
-- toRender (Node (RAnnot (OpacityGroup x)) rs)
--                                 = R $ do
--   let R r = foldMap toRender rs
--   P.opacityGroup x r

