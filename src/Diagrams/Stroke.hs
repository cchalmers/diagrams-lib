{-# LANGUAGE FlexibleContexts #-}
import Diagrams.Core
import Diagrams.Path
import qualified Diagrams.Path.TwoD as T
import Data.Monoid

class Stroke v n where
  data StrokeOpts v n

  stroke :: (InSpace v n t, ToPath t, Renderable (Path v n) b)
         => t -> QDiagram b v n Any
  stroke = strokePath . toPath

  stroke' :: (InSpace v n t, ToPath t, Renderable (Path v n) b)
          => StrokeOpts v n -> t -> QDiagram b v n Any

  strokePath   :: Renderable (Path v n) b => Path v n -> QDiagram b v n Any
  strokePath'  :: Renderable (Path v n) b
               => StrokeOpts v n -> Path v n -> QDiagram b v n Any

  strokeTrail  :: Renderable (Path v n) b => Trail v n -> QDiagram b v n Any
  strokeTrail = strokePath . toPath
  strokeTrail' :: Renderable (Path v n) b => Trail v n -> QDiagram b v n Any

  strokeLine  :: Renderable (Path v n) b => Trail' Line v n -> QDiagram b v n Any
  strokeLine = strokePath . toPath
  strokeLine' :: Renderable (Path v n) b => Trail' Line v n -> QDiagram b v n Any

  strokeLoop  :: Renderable (Path v n) b => Trail' Loop v n -> QDiagram b v n Any
  strokeLoop = strokePath . toPath
  strokeLoop' :: Renderable (Path v n) b => Trail' Loop v n -> QDiagram b v n Any

instance TypeableFloat n => Stroke V2 n where
  data StrokeOpts = StrokeOpts
    { _vertexNames    :: [Name]
    , _strokeFillRule :: FileRule
    }


-- | Convert a 'ToPath' object into a diagram.  The resulting diagram has the
--   names 0, 1, ... assigned to each of the path's vertices.
--
--   See also 'stroke'', which takes an extra options record allowing
--   its behaviour to be customized.
stroke :: (InSpace V2 n t, ToPath t, TypeableFloat n, Renderable (Path V2 n) b)
       => t -> QDiagram b V2 n Any
stroke = strokeP . toPath

-- | A variant of 'stroke' that takes an extra record of options to
--   customize its behaviour.  In particular:
--
--     * Names can be assigned to the path's vertices
--
--   'StrokeOpts' is an instance of 'Default', so @stroke' ('with' &
--   ... )@ syntax may be used.
stroke' :: (InSpace V2 n t, ToPath t, TypeableFloat n, Renderable (Path V2 n) b, IsName a)
       => StrokeOpts a -> t -> QDiagram b V2 n Any
stroke' opts = strokeP' opts . toPath

-- | 'stroke' specialised to 'Path'.
strokeP :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Path V2 n -> QDiagram b V2 n Any
strokeP = strokeP' (def :: StrokeOpts ())

-- | 'stroke' specialised to 'Path'.
strokePath :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Path V2 n -> QDiagram b V2 n Any
strokePath = strokeP

instance (TypeableFloat n, Renderable (Path V2 n) b)
    => TrailLike (QDiagram b V2 n Any) where
  trailLike = strokeP . trailLike

-- | 'stroke'' specialised to 'Path'.
strokeP' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
    => StrokeOpts a -> Path V2 n -> QDiagram b V2 n Any
strokeP' opts path
  | null (pLines ^. _Wrapped') = mkP pLoops
  | null (pLoops ^. _Wrapped') = mkP pLines
  | otherwise                  = mkP pLines <> mkP pLoops
  where
    (pLines,pLoops) = partitionPath (isLine . unLoc) path
    mkP p
      = mkQD (Prim p)
         (getEnvelope p)
         (getTrace p)
         (fromNames . concat $
           zipWith zip (opts^.vertexNames) ((map . map) subPoint (pathVertices p))
         )
         (Query $ Any . flip (runFillRule (opts^.queryFillRule)) p)

-- | 'stroke'' specialised to 'Path'.
strokePath' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
    => StrokeOpts a -> Path V2 n -> QDiagram b V2 n Any
strokePath' = strokeP'

-- | 'stroke' specialised to 'Trail'.
strokeTrail :: (TypeableFloat n, Renderable (Path V2 n) b)
            => Trail V2 n -> QDiagram b V2 n Any
strokeTrail = stroke . pathFromTrail

-- | 'stroke' specialised to 'Trail'.
strokeT :: (TypeableFloat n, Renderable (Path V2 n) b)
        => Trail V2 n -> QDiagram b V2 n Any
strokeT = strokeTrail

-- | A composition of 'stroke'' and 'pathFromTrail' for conveniently
--   converting a trail directly into a diagram.
strokeTrail' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
             => StrokeOpts a -> Trail V2 n -> QDiagram b V2 n Any
strokeTrail' opts = stroke' opts . pathFromTrail

-- | Deprecated synonym for 'strokeTrail''.
strokeT' :: (TypeableFloat n, Renderable (Path V2 n) b, IsName a)
         => StrokeOpts a -> Trail V2 n -> QDiagram b V2 n Any
strokeT' = strokeTrail'

-- | A composition of 'strokeT' and 'wrapLine' for conveniently
--   converting a line directly into a diagram.
strokeLine :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Trail' Line V2 n -> QDiagram b V2 n Any
strokeLine = strokeT . wrapLine

-- | A composition of 'strokeT' and 'wrapLoop' for conveniently
--   converting a loop directly into a diagram.
strokeLoop :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Trail' Loop V2 n -> QDiagram b V2 n Any
strokeLoop = strokeT . wrapLoop

-- | A convenience function for converting a @Located Trail@ directly
--   into a diagram; @strokeLocTrail = stroke . trailLike@.
strokeLocTrail :: (TypeableFloat n, Renderable (Path V2 n) b)
               => Located (Trail V2 n) -> QDiagram b V2 n Any
strokeLocTrail = strokeP . trailLike

-- | Deprecated synonym for 'strokeLocTrail'.
strokeLocT :: (TypeableFloat n, Renderable (Path V2 n) b)
           => Located (Trail V2 n) -> QDiagram b V2 n Any
strokeLocT = strokeLocTrail

-- | A convenience function for converting a @Located@ line directly
--   into a diagram; @strokeLocLine = stroke . trailLike . mapLoc wrapLine@.
strokeLocLine :: (TypeableFloat n, Renderable (Path V2 n) b)
              => Located (Trail' Line V2 n) -> QDiagram b V2 n Any
strokeLocLine = strokeP . trailLike . mapLoc wrapLine

-- | A convenience function for converting a @Located@ loop directly
--   into a diagram; @strokeLocLoop = stroke . trailLike . mapLoc wrapLoop@.
strokeLocLoop :: (TypeableFloat n, Renderable (Path V2 n) b)
              => Located (Trail' Loop V2 n) -> QDiagram b V2 n Any
strokeLocLoop = strokeP . trailLike . mapLoc wrapLoop
