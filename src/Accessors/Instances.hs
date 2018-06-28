{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Accessors.Instances () where

import Control.Compose ( (:.)(..), Id(..), unO, unId )
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
  toAccessorTree lens0 =
    Right $
    GAData "(,)" $
    GAConstructor "(,)"
    [ (Just "(x,_)", toAccessorTree (lens0 . lens1))
    , (Just "(_,x)", toAccessorTree (lens0 . lens2))
    ]
    where
      lens1 ::  Lens' (a, b) a
      lens1 f (x, y) = fmap (\x' -> (x', y)) (f x)

      lens2 :: Lens' (a, b) b
      lens2 f (x, y) = fmap (\y' -> (x, y')) (f y)

instance (Lookup a, Lookup b, Lookup c) => Lookup (a, b, c) where
  toAccessorTree lens0 =
    Right $
    GAData "(,,)" $
    GAConstructor "(,,)"
    [ (Just "(x,_,_)", toAccessorTree (lens0 . lens1))
    , (Just "(_,x,_)", toAccessorTree (lens0 . lens2))
    , (Just "(_,_,x)", toAccessorTree (lens0 . lens3))
    ]
    where
      lens1 ::  Lens' (a, b, c) a
      lens1 f (x, y, z) = fmap (\x' -> (x', y, z)) (f x)
      lens2 :: Lens' (a, b, c) b
      lens2 f (x, y, z) = fmap (\y' -> (x, y', z)) (f y)
      lens3 :: Lens' (a, b, c) c
      lens3 f (x, y, z) = fmap (\z' -> (x, y, z')) (f z)

instance (Lookup a, Lookup b, Lookup c, Lookup d) => Lookup (a, b, c, d) where
  toAccessorTree lens0 =
    Right $
    GAData "(,,,)" $
    GAConstructor "(,,,)"
    [ (Just "(x,_,_,_)", toAccessorTree (lens0 . lens1))
    , (Just "(_,x,_,_)", toAccessorTree (lens0 . lens2))
    , (Just "(_,_,x,_)", toAccessorTree (lens0 . lens3))
    , (Just "(_,_,_,x)", toAccessorTree (lens0 . lens4))
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

-- TODO(MP): Product?  Const?  Identity?

-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toAccessorTree _ =
    Right $ GAData "V0" $ GAConstructor "V0" []
instance Lookup a => Lookup (Linear.V1 a) where
  toAccessorTree lens0 =
    Right $ GAData "V1" $ GAConstructor "V1"
    [(Just "x", toAccessorTree (lens0 . Linear._x))]

instance Lookup a => Lookup (Linear.V2 a) where
  toAccessorTree lens0 =
    Right $ GAData "V2" $ GAConstructor "V2"
    [ (Just "x", toAccessorTree (lens0 . Linear._x))
    , (Just "y", toAccessorTree (lens0 . Linear._y))
    ]

instance Lookup a => Lookup (Linear.V3 a) where
  toAccessorTree lens0 =
    Right $ GAData "V3" $ GAConstructor "V3"
    [ (Just "x", toAccessorTree (lens0 . Linear._x))
    , (Just "y", toAccessorTree (lens0 . Linear._y))
    , (Just "z", toAccessorTree (lens0 . Linear._z))
    ]
instance Lookup a => Lookup (Linear.V4 a) where
  toAccessorTree lens0 =
    Right $ GAData "V4" $ GAConstructor "V4"
    [ (Just "x", toAccessorTree (lens0 . Linear._x))
    , (Just "y", toAccessorTree (lens0 . Linear._y))
    , (Just "z", toAccessorTree (lens0 . Linear._z))
    , (Just "w", toAccessorTree (lens0 . Linear._w))
    ]
instance Lookup a => Lookup (Linear.Quaternion a) where
  toAccessorTree lens0 =
    Right $ GAData "Quaternion" $ GAConstructor "Quaternion"
    [ (Just "q0", toAccessorTree (lens0 . Linear._e))
    , (Just "q1", toAccessorTree (lens0 . Linear._i))
    , (Just "q2", toAccessorTree (lens0 . Linear._j))
    , (Just "q3", toAccessorTree (lens0 . Linear._k))
    ]

-- basic types
instance Lookup () where -- hack to get dummy tree
  toAccessorTree _ = Left FieldSorry
instance Lookup Int8 where
  toAccessorTree lens = Left (FieldInt8 lens)
instance Lookup Int16 where
  toAccessorTree lens = Left (FieldInt16 lens)
instance Lookup Int32 where
  toAccessorTree lens = Left (FieldInt32 lens)
instance Lookup Int64 where
  toAccessorTree lens = Left (FieldInt64 lens)
instance Lookup Word8 where
  toAccessorTree lens = Left (FieldWord8 lens)
instance Lookup Word16 where
  toAccessorTree lens = Left (FieldWord16 lens)
instance Lookup Word32 where
  toAccessorTree lens = Left (FieldWord32 lens)
instance Lookup Word64 where
  toAccessorTree lens = Left (FieldWord64 lens)
instance Lookup Float where
  toAccessorTree lens = Left (FieldFloat lens)
instance Lookup Double where
  toAccessorTree lens = Left (FieldDouble lens)
instance Lookup Bool
instance Lookup String where
  toAccessorTree lens = Left (FieldString lens)

-- Word types
instance Lookup Word where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))

-- Int types
instance Lookup Int where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))

-- C types
-- todo(greg): some of these have inappropriate fields
instance Lookup CChar where
  toAccessorTree lens0 = Left (FieldInt8 (lens0 . integralLens))
instance Lookup CSChar where
  toAccessorTree lens0 = Left (FieldInt8 (lens0 . integralLens))
instance Lookup CUChar where
  toAccessorTree lens0 = Left (FieldWord8 (lens0 . integralLens))
instance Lookup CShort where
  toAccessorTree lens0 = Left (FieldInt16 (lens0 . integralLens))
instance Lookup CUShort where
  toAccessorTree lens0 = Left (FieldWord16 (lens0 . integralLens))
instance Lookup CInt where
  toAccessorTree lens0 = Left (FieldInt32 (lens0 . integralLens))
instance Lookup CUInt where
  toAccessorTree lens0 = Left (FieldWord32 (lens0 . integralLens))
instance Lookup CLong where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))
instance Lookup CULong where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))
instance Lookup CPtrdiff where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))
instance Lookup CSize where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))
instance Lookup CWchar where
  toAccessorTree lens0 = Left (FieldInt32 (lens0 . integralLens))
instance Lookup CSigAtomic where
  toAccessorTree lens0 = Left (FieldInt32 (lens0 . integralLens))
instance Lookup CLLong where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))
instance Lookup CULLong where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))
instance Lookup CIntPtr where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))
instance Lookup CUIntPtr where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))
instance Lookup CIntMax where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . integralLens))
instance Lookup CUIntMax where
  toAccessorTree lens0 = Left (FieldWord64 (lens0 . integralLens))
instance Lookup CClock where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . clockLens))
    where
      clockLens f (CClock x) = fmap (CClock . fromIntegral) (f (fromIntegral x))
instance Lookup CTime where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . timeLens))
    where
      timeLens f (CTime x) = fmap (CTime . fromIntegral) (f (fromIntegral x))
instance Lookup CUSeconds where
  toAccessorTree lens0 = Left (FieldWord32 (lens0 . usecondsLens))
    where
      usecondsLens f (CUSeconds x) = fmap (CUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CSUSeconds where
  toAccessorTree lens0 = Left (FieldInt64 (lens0 . susecondsLens))
    where
      susecondsLens f (CSUSeconds x) = fmap (CSUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CFloat where
  toAccessorTree lens0 = Left (FieldDouble (lens0 . realFracLens))
instance Lookup CDouble where
  toAccessorTree lens0 = Left (FieldDouble (lens0 . realFracLens))

{-# INLINE integralLens #-}
integralLens :: (Integral a, Integral b) => Lens' a b
integralLens f x = fmap fromIntegral (f (fromIntegral x))

{-# INLINE realFracLens #-}
realFracLens :: (Fractional a, Real a) => Lens' a Double
realFracLens f x = fmap realToFrac (f (realToFrac x))

-- other types
instance Lookup a => Lookup (Id a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap Id (f (unId x))))
instance Lookup (g (f a)) => Lookup ((g :. f) a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap O (f (unO x))))
instance Lookup (g a) => Lookup (Rot f1 f2 g a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap Rot (f (unRot x))))
instance Lookup a => Lookup (V3T f a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap V3T (f (unV x))))
instance Lookup a => Lookup (Euler a)
