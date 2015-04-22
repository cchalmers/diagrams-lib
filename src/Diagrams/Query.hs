-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Query
-- Copyright   :  (c) 2013-2015 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A query is a function that maps points in a vector space to values
-- in some monoid. Queries naturally form a monoid, with two queries
-- being combined pointwise.
--
-----------------------------------------------------------------------------

module Diagrams.Query
  (
    -- * Queries
    Query(..)
  , query
  -- , queryPoint
  , sample
  , value
  , resetValue
  , clearValue
  ) where

import           Data.Monoid
import           Diagrams.Core
import           Control.Lens

-- | Sample a diagram's query function at a given point.
sample :: Monoid m => QDiagram b v n m -> Point v n -> m
sample = runQuery . view query

-- | Set the query value for 'True' points in a diagram (/i.e./ points
--   \"inside\" the diagram); 'False' points will be set to 'mempty'.
value :: Monoid m => m -> QDiagram b v n Any -> QDiagram b v n m
value m = fmap fromAny
  where fromAny (Any True)  = m
        fromAny (Any False) = mempty

-- | Reset the query values of a diagram to @True@/@False@: any values
--   equal to 'mempty' are set to 'False'; any other values are set to
--   'True'.
resetValue :: (Eq m, Monoid m) => QDiagram b v n m -> QDiagram b v n Any
resetValue = fmap toAny
  where toAny m | m == mempty = Any False
                | otherwise   = Any True

-- | Set all the query values of a diagram to 'False'.
clearValue :: QDiagram b v n m -> QDiagram b v n Any
clearValue = (<$) mempty

