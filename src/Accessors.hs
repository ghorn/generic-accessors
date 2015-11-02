{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Accessors
       ( Lookup(..)
       , AccessorTree(..)
       , Field(..)
       , accessors
       , describeField
       , flatten
       , flatten'
       , showTree
       , showFlat
       ) where

import GHC.Generics

import Control.Lens ( Lens', (^.) )
import Data.List ( intercalate )
import qualified Linear
import GHC.Word
import Data.Int
import Foreign.C.Types

import SpatialMath ( Euler )
import SpatialMathT ( V3T(..), Rot(..) )

showAccTree :: String -> AccessorTree a -> [String]
showAccTree spaces (Field _) = [spaces ++ "Field {}"]
showAccTree spaces (Data name trees) =
  (spaces ++ "Data " ++ show name) :
  concatMap (showChild (spaces ++ "    ")) trees

showChild :: String -> (String, AccessorTree a) -> [String]
showChild spaces (name, tree) =
  (spaces ++ name) : showAccTree (spaces ++ "    ") tree

instance Show (AccessorTree a) where
  show = unlines . showAccTree ""

data AccessorTree a = Data (String,String) [(String, AccessorTree a)]
                    | Field (Field a)

data Field a =
  FieldBool (Lens' a Bool)
  | FieldDouble (Lens' a Double)
  | FieldFloat (Lens' a Float)
  | FieldInt (Lens' a Int)
  | FieldString (Lens' a String)
  | FieldSorry -- ^ a field which is not yet supported

-- | Return the type of field, such as "Bool", "Double", "String", etc.
describeField :: Field a -> String
describeField (FieldBool _) = "Bool"
describeField (FieldDouble _) = "Double"
describeField (FieldFloat _) = "Float"
describeField (FieldInt _) = "Int"
describeField (FieldString _) = "String"
describeField FieldSorry = "Sorry"

accessors :: Lookup a => AccessorTree a
accessors = toAccessorTree id

showMsgs :: [String] -> String
showMsgs = intercalate "."

flatten :: AccessorTree a -> [(String, Field a)]
flatten = map f . flatten'
  where
    f (x,y) = (showMsgs x, y)

flatten' :: AccessorTree a -> [([String], Field a)]
flatten' = flattenChain []
  where
    flattenChain :: [String] -> AccessorTree a -> [([String], Field a)]
    flattenChain msgs (Field lens) = [(reverse msgs, lens)]
    flattenChain msgs (Data (_,_) trees) = concatMap f trees
      where
        f (name,tree) = flattenChain (name:msgs) tree

-- | Things which you can make a tree of labeled getters for.
-- You should derive this using GHC.Generics.
class Lookup a where
  toAccessorTree :: Lens' b a -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a))
                            => Lens' b a -> AccessorTree b
  toAccessorTree lens0 = gtoAccessorTree (lens0 . repLens)
    where
      repLens :: Lens' a (Rep a p)
      repLens f y = fmap to (f (from y))

class GLookup f where
  gtoAccessorTree :: Lens' b (f a) -> AccessorTree b

class GLookupS f where
  gtoAccessorTreeS :: Lens' b (f a)
                      -> [(String, AccessorTree b)]

instance Lookup f => GLookup (Rec0 f) where
  gtoAccessorTree lens0 = toAccessorTree (lens0 . rec0Lens)
    where
      rec0Lens :: Lens' (Rec0 f a) f
      rec0Lens f y = fmap K1 (f (unK1 y))

instance (Selector s, GLookup a) => GLookupS (S1 s a) where
  gtoAccessorTreeS lens0 = [(selname, gtoAccessorTree (lens0 . m1Lens))]
    where
      m1Lens :: Lens' (S1 s f p) (f p)
      m1Lens f y = fmap M1 (f (unM1 y))

      selname = case selName selError of
        "" -> "()"
        y -> y

      selError :: S1 s a p
      selError = error $ "generic-accessors: selName should never access data"


instance GLookupS U1 where
  gtoAccessorTreeS _ = []

instance (GLookupS f, GLookupS g) => GLookupS (f :*: g) where
  gtoAccessorTreeS lens0 = tf ++ tg
    where
      tf = gtoAccessorTreeS (lens0 . leftLens)
      tg = gtoAccessorTreeS (lens0 . rightLens)

      leftLens ::  Lens' ((f :*: g) a) (f a)
      leftLens  f (x :*: y) = fmap (\x' -> x' :*: y ) (f x)
      rightLens :: Lens' ((f :*: g) a) (g a)
      rightLens f (x :*: y) = fmap (\y' -> x  :*: y') (f y)

instance (Datatype d, Constructor c, GLookupS a)
         => GLookup (D1 d (C1 c a)) where
  gtoAccessorTree lens0 = Data (datatypeName datatypeError, conName conError) con
    where
      conError :: C1 c a p
      conError = error $ "generic-accessors: conName should never access data"

      datatypeError :: D1 d (C1 c a) p
      datatypeError = error $ "generic-accessors: datatypeName should never access data"

      con = gtoAccessorTreeS (lens0 . m1m1Lens)

      m1m1Lens :: Lens' (D1 d (C1 c f) p) (f p)
      m1m1Lens f y = fmap (M1 . M1) (f (unM1 (unM1 y)))

-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toAccessorTree _ =
    Data ("V0", "V0") []
instance Lookup a => Lookup (Linear.V1 a) where
  toAccessorTree lens0 =
    Data ("V1", "V1") [ ("x", toAccessorTree (lens0 . Linear._x))
                      ]

instance Lookup a => Lookup (Linear.V2 a) where
  toAccessorTree lens0 =
    Data ("V2", "V2") [ ("x", toAccessorTree (lens0 . Linear._x))
                      , ("y", toAccessorTree (lens0 . Linear._y))
                      ]

instance Lookup a => Lookup (Linear.V3 a) where
  toAccessorTree lens0 =
    Data ("V3", "V3") [ ("x", toAccessorTree (lens0 . Linear._x))
                      , ("y", toAccessorTree (lens0 . Linear._y))
                      , ("z", toAccessorTree (lens0 . Linear._z))
                      ]
instance Lookup a => Lookup (Linear.V4 a) where
  toAccessorTree lens0 =
    Data ("V4", "V4") [ ("x", toAccessorTree (lens0 . Linear._x))
                      , ("y", toAccessorTree (lens0 . Linear._y))
                      , ("z", toAccessorTree (lens0 . Linear._z))
                      , ("w", toAccessorTree (lens0 . Linear._w))
                      ]
instance Lookup a => Lookup (Linear.Quaternion a) where
  toAccessorTree lens0 =
    Data ("Quaternion", "Quaternion")
    [ ("q0", toAccessorTree (lens0 . Linear._e))
    , ("q1", toAccessorTree (lens0 . Linear._i))
    , ("q2", toAccessorTree (lens0 . Linear._j))
    , ("q3", toAccessorTree (lens0 . Linear._k))
    ]

-- basic types
instance Lookup () where -- hack to get dummy tree
  toAccessorTree _ = Field FieldSorry
instance Lookup Int where
  toAccessorTree lens = Field (FieldInt lens)
instance Lookup Float where
  toAccessorTree lens = Field (FieldFloat lens)
instance Lookup Double where
  toAccessorTree lens = Field (FieldDouble lens)
instance Lookup Bool where
  toAccessorTree lens = Field (FieldBool lens)
instance Lookup String where
  toAccessorTree lens = Field (FieldString lens)

-- Word types
instance Lookup Word where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Word8 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Word16 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Word32 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Word64 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))

-- Int types
instance Lookup Int8 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Int16 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Int32 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup Int64 where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))

-- C types
-- todo(greg): some of these have inappropriate fields
instance Lookup CChar where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CSChar where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CUChar where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CShort where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CUShort where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CInt where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CUInt where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CLong where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CULong where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CPtrdiff where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CSize where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CWchar where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CSigAtomic where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CLLong where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CULLong where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CIntPtr where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CUIntPtr where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CIntMax where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CUIntMax where
  toAccessorTree lens0 = Field (FieldInt (lens0 . integralLens))
instance Lookup CClock where
  toAccessorTree lens0 = Field (FieldInt (lens0 . clockLens))
    where
      clockLens f (CClock x) = fmap (CClock . fromIntegral) (f (fromIntegral x))
instance Lookup CTime where
  toAccessorTree lens0 = Field (FieldInt (lens0 . timeLens))
    where
      timeLens f (CTime x) = fmap (CTime . fromIntegral) (f (fromIntegral x))
instance Lookup CUSeconds where
  toAccessorTree lens0 = Field (FieldInt (lens0 . usecondsLens))
    where
      usecondsLens f (CUSeconds x) = fmap (CUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CSUSeconds where
  toAccessorTree lens0 = Field (FieldInt (lens0 . susecondsLens))
    where
      susecondsLens f (CSUSeconds x) = fmap (CSUSeconds . fromIntegral) (f (fromIntegral x))
instance Lookup CFloat where
  toAccessorTree lens0 = Field (FieldDouble (lens0 . realFracLens))
instance Lookup CDouble where
  toAccessorTree lens0 = Field (FieldDouble (lens0 . realFracLens))

{-# INLINE integralLens #-}
integralLens :: Integral a => Lens' a Int
integralLens f x = fmap fromIntegral (f (fromIntegral x))

{-# INLINE realFracLens #-}
realFracLens :: (Fractional a, Real a) => Lens' a Double
realFracLens f x = fmap realToFrac (f (realToFrac x))

-- other types
instance Lookup a => Lookup (Rot f1 f2 a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap Rot (f (unR x))))
instance Lookup a => Lookup (V3T f a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . (\f x -> fmap V3T (f (unV x))))
instance Lookup a => Lookup (Euler a)

showAccTrees :: (Double -> String) -> a -> [(String, AccessorTree a)] -> String -> [String]
showAccTrees show' x trees spaces = concat cs ++ [spaces ++ "}"]
  where
    cs = zipWith (showRecordField show' x spaces) trees ("{ " : repeat ", ")

showVal :: Field a -> (Double -> String) -> a -> String
showVal (FieldBool lens) _ x = show (x ^. lens)
showVal (FieldInt lens) _ x = show (x ^. lens)
showVal (FieldDouble lens) show' x = show' (x ^. lens)
showVal (FieldFloat lens) show' x = show' (realToFrac (x ^. lens))
showVal (FieldString lens) _ x = x ^. lens
showVal FieldSorry _ _ = ""

showRecordField :: (Double -> String) -> a -> String -> (String, AccessorTree a) -> String -> [String]
showRecordField show' x spaces (getterName, (Field field)) prefix =
  [spaces ++ prefix ++ getterName ++ " = " ++ showVal field show' x]
showRecordField show' x spaces (getterName, Data (_,cons) trees) prefix =
  (spaces ++ prefixNameEq ++ cons) : showAccTrees show' x trees newSpaces
  where
    prefixNameEq = prefix ++ getterName ++ " = "
    newSpaces = spaces ++ (replicate (length prefixNameEq) ' ')

-- version of (init . unlines) which doesn't throw an error on empty input
initUnlines :: [String] -> [Char]
initUnlines [] = ""
initUnlines xs = init (unlines xs)

-- | Show a tree of values
showTree :: AccessorTree a -> (Double -> String) -> a -> String
showTree (Data (_,cons) trees) show' x = initUnlines $ cons : showAccTrees show' x trees ""
showTree (Field field) show' x = showVal field show' x

-- | Show a list of values
-- .
-- True --> align the colums, False --> total mayhem
showFlat :: forall a . AccessorTree a -> Bool -> (Double -> String) -> a -> String
showFlat at align show' x = initUnlines $ map f fl
  where
    n = maximum (map (length . fst) fl)

    f (name, lens) = name ++ spaces ++ " = " ++ showVal lens show' x
      where
        spaces
          | align = replicate (n - length name) ' '
          | otherwise = ""

    fl :: [(String, Field a)]
    fl = flatten at
