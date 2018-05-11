module Refined
  ( Refined
  , refine
  , refineTH
  , unrefine
  
  , Predicate(..)
  , Weaken(..)

  , Size
  , NonEmpty
  , Ascending
  , Descending

  , Not
  , And
  , Or

  , LessThan
  , GreaterThan
  , From
  , To
  , FromTo
  , EqualTo
  
  , Positive
  , NonPositive
  , Negative
  , NonNegative
  , ZeroToOne
  ) where

import Refined.Internal
import Refined.Orphan () -- instances
