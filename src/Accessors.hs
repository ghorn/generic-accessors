{-# OPTIONS_GHC -Wall #-}

module Accessors
       ( Lookup(..)
       , AccessorTree
       , GAData(..)
       , GAConstructor(..)
       , GASimpleEnum(..)
       , GAField(..)
       , GATip(..)
       , accessors
       , describeGAField
       , sameFieldType
       , flatten
       , flatten'
       , showTree
       , showFlat
       , GLookup(..)
       , GEnum(..)
       ) where

import Accessors.Accessors
import Accessors.Instances ()
