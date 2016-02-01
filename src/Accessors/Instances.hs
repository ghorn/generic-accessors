{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Accessors.Instances () where

import Control.Lens ( Lens' )
import qualified Linear
import GHC.Word
import Data.Int
import Foreign.C.Types
import SpatialMath ( Euler )
import SpatialMathT ( V3T(..), Rot(..) )

import Accessors.Accessors ( Lookup(..), GAData(..), GAConstructor(..), GAField(..) )

-- tuple instances
instance (Lookup a, Lookup b) => Lookup (a, b) where
  toGAData lens0 =
    Right $
    GAData "(,)" $
    GAConstructor "(,)"
    [ (Just "(x,_)", toGAData (lens0 . lens1))
    , (Just "(_,x)", toGAData (lens0 . lens2))
    ]
    where
      lens1 ::  Lens' (a, b) a
      lens1 f (x, y) = fmap (\x' -> (x', y)) (f x)

      lens2 :: Lens' (a, b) b
      lens2 f (x, y) = fmap (\y' -> (x, y')) (f y)

instance (Lookup a, Lookup b, Lookup c) => Lookup (a, b, c) where
  toGAData lens0 =
    Right $
    GAData "(,,)" $
    GAConstructor "(,,)"
    [ (Just "(x,_,_)", toGAData (lens0 . lens1))
    , (Just "(_,x,_)", toGAData (lens0 . lens2))
    , (Just "(_,_,x)", toGAData (lens0 . lens3))
    ]
    where
      lens1 ::  Lens' (a, b, c) a
      lens1 f (x, y, z) = fmap (\x' -> (x', y, z)) (f x)
      lens2 :: Lens' (a, b, c) b
      lens2 f (x, y, z) = fmap (\y' -> (x, y', z)) (f y)
      lens3 :: Lens' (a, b, c) c
      lens3 f (x, y, z) = fmap (\z' -> (x, y, z')) (f z)

instance (Lookup a, Lookup b, Lookup c, Lookup d) => Lookup (a, b, c, d) where
  toGAData lens0 =
    Right $
    GAData "(,,,)" $
    GAConstructor "(,,,)"
    [ (Just "(x,_,_,_)", toGAData (lens0 . lens1))
    , (Just "(_,x,_,_)", toGAData (lens0 . lens2))
    , (Just "(_,_,x,_)", toGAData (lens0 . lens3))
    , (Just "(_,_,_,x)", toGAData (lens0 . lens4))
    ]
    where
      lens1 ::  Lens' (a, b, c, d) a
      lens1 f (x, y, z, w) = fmap (\x' -> (x', y, z, w)) (f x)
      lens2 :: Lens' (a, b, c, d) b
      lens2 f (x, y, z, w) = fmap (\y' -> (x, y', z, w)) (f y)
      lens3 :: Lens' (a, b, c, d) c
      lens3 f (x, y, z, w) = fmap (\z' -> (x, y, z', w)) (f z)
      lens4 :: Lens' (a, b, c, d) d
      lens4 f (x, y, z, w) = fmap (\w' -> (x, y, z, w')) (f w)


-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toGAData _ =
    Right $ GAData "V0" $ GAConstructor "V0" []
instance Lookup a => Lookup (Linear.V1 a) where
  toGAData lens0 =
    Right $ GAData "V1" $ GAConstructor "V1"
    [(Just "x", toGAData (lens0 . Linear._x))]

instance Lookup a => Lookup (Linear.V2 a) where
  toGAData lens0 =
    Right $ GAData "V2" $ GAConstructor "V2"
    [ (Just "x", toGAData (lens0 . Linear._x))
    , (Just "y", toGAData (lens0 . Linear._y))
    ]

instance Lookup a => Lookup (Linear.V3 a) where
  toGAData lens0 =
    Right $ GAData "V3" $ GAConstructor "V3"
    [ (Just "x", toGAData (lens0 . Linear._x))
    , (Just "y", toGAData (lens0 . Linear._y))
    , (Just "z", toGAData (lens0 . Linear._z))
    ]
instance Lookup a => Lookup (Linear.V4 a) where
  toGAData lens0 =
    Right $ GAData "V4" $ GAConstructor "V4"
    [ (Just "x", toGAData (lens0 . Linear._x))
    , (Just "y", toGAData (lens0 . Linear._y))
    , (Just "z", toGAData (lens0 . Linear._z))
    , (Just "w", toGAData (lens0 . Linear._w))
    ]
instance Lookup a => Lookup (Linear.Quaternion a) where
  toGAData lens0 =
    Right $ GAData "Quaternion" $ GAConstructor "Quaternion"
    [ (Just "q0", toGAData (lens0 . Linear._e))
    , (Just "q1", toGAData (lens0 . Linear._i))
    , (Just "q2", toGAData (lens0 . Linear._j))
    , (Just "q3", toGAData (lens0 . Linear._k))
    ]

-- basic types
instance Lookup () where -- hack to get dummy tree
  toGAData _ = Left FieldSorry
instance Lookup Int where
  toGAData lens = Left (FieldInt lens)
instance Lookup Float where
  toGAData lens = Left (FieldFloat lens)
instance Lookup Double where
  toGAData lens = Left (FieldDouble lens)
instance Lookup Bool
instance Lookup String where
  toGAData lens = Left (FieldString lens)

-- Word types
instance Lookup Word where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Word8 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Word16 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Word32 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Word64 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))

-- Int types
instance Lookup Int8 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Int16 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Int32 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup Int64 where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))

-- C types
-- todo(greg): some of these have inappropriate fields
instance Lookup CChar where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CSChar where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CUChar where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CShort where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CUShort where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CInt where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CUInt where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CLong where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CULong where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CPtrdiff where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CSize where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CWchar where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CSigAtomic where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CLLong where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CULLong where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CIntPtr where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CUIntPtr where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CIntMax where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CUIntMax where
  toGAData lens0 = Left (FieldInt (lens0 . integralLens))
instance Lookup CClock where
  toGAData lens0 = Left (FieldInt (lens0 . clockLens))
    where
      clockLens f (CClock x) = fmap (CClock . fromIntegral) (f (fromIntegral x))
instance Lookup CTime where
  toGAData lens0 = Left (FieldInt (lens0 . timeLens))
    where
      timeLens f (CTime x) = fmap (CTime . fromIntegral) (f (fromIntegral x))
instance Lookup CUSeconds where
  toGAData lens0 = Left (FieldInt (lens0 . usecondsLens))
    where
      usecondsLens f (CUSeconds x) = fmap (CUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CSUSeconds where
  toGAData lens0 = Left (FieldInt (lens0 . susecondsLens))
    where
      susecondsLens f (CSUSeconds x) = fmap (CSUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CFloat where
  toGAData lens0 = Left (FieldDouble (lens0 . realFracLens))
instance Lookup CDouble where
  toGAData lens0 = Left (FieldDouble (lens0 . realFracLens))

{-# INLINE integralLens #-}
integralLens :: Integral a => Lens' a Int
integralLens f x = fmap fromIntegral (f (fromIntegral x))

{-# INLINE realFracLens #-}
realFracLens :: (Fractional a, Real a) => Lens' a Double
realFracLens f x = fmap realToFrac (f (realToFrac x))

-- other types
instance Lookup a => Lookup (Rot f1 f2 a) where
  toGAData lens0 = toGAData (lens0 . (\f x -> fmap Rot (f (unR x))))
instance Lookup a => Lookup (V3T f a) where
  toGAData lens0 = toGAData (lens0 . (\f x -> fmap V3T (f (unV x))))
instance Lookup a => Lookup (Euler a)
