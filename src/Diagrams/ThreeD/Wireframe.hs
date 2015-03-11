module Diagrams.ThreeD.Wireframe where

import Diagrams.BoundingBox
import Data.IntMap as IM
import Diagrams.Path
import Diagrams.Core
import Data.Maybe
import Data.Semigroup
import Diagrams.TrailLike
import Diagrams.ThreeD.Types
import Control.Applicative
-- import Control.Lens

import Data.Foldable (foldMap)

makeWireFrame :: OrderedField n => IntMap (P3 n) -> [(Int,Int)] -> Path V3 n
makeWireFrame im = foldMap (uncurry mkEdge)
  where
    mkEdge a b = fromMaybe mempty $ do
      v1 <- IM.lookup a im
      v2 <- IM.lookup b im
      pure $ v1 ~~ v2

cubeFrame :: OrderedField n => Path V3 n
cubeFrame = makeWireFrame im edges
  where
    im = IM.fromList . zip [1..] . getAllCorners $ fromCorners (-1) 1
    edges = zip [1,2,3,1,5,6,7,8,1,2,7,4]
                [2,4,4,3,7,5,8,6,5,6,3,8]
    -- edges = zip [1,2,3,3,4,4,5,6,7,7,8,8]
    --             [6,5,2,8,1,7,1,2,3,6,4,5]


