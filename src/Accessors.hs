{-# OPTIONS_GHC -Wall #-}

module Accessors
       ( Lookup(..)
       , GAData(..)
       , GAConstructor(..)
       , GASimpleEnum(..)
       , GAField(..)
       , accessors
       , describeGAField
       , sameFieldType
       , flatten
       , flatten'
       , showTree
       , showFlat
       ) where

import Accessors.Accessors
import Accessors.Instances ()
