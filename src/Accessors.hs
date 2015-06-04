{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Accessors
       ( Lookup(..)
       , AccessorTree(..)
       , Setter(..)
       , Getter(..)
       , accessors
       , flatten
       , showTree
       , showFlat
       ) where

import GHC.Generics

import Data.List ( intercalate )
import qualified Linear
import GHC.Word
import Data.Int
import Foreign.C.Types

import SpatialMath ( Euler )
import SpatialMathT ( V3T(..), Rot(..) )

showAccTree :: String -> AccessorTree a -> [String]
showAccTree spaces (ATGetter _) = [spaces ++ "ATGetter {}"]
showAccTree spaces (Data name trees) =
  (spaces ++ "Data " ++ show name) :
  concatMap (showChild (spaces ++ "    ")) trees

showChild :: String -> (String, AccessorTree a) -> [String]
showChild spaces (name, tree) =
  (spaces ++ name) : showAccTree (spaces ++ "    ") tree

instance Show (AccessorTree a) where
  show = unlines . showAccTree ""

data AccessorTree a = Data (String,String) [(String, AccessorTree a)]
                    | ATGetter (Getter a, Setter a)

data Getter a =
  GetBool (a -> Bool)
  | GetDouble (a -> Double)
  | GetFloat (a -> Float)
  | GetInt (a -> Int)
  | GetSorry -- ^ not yet implemented

data Setter a =
  SetBool (Bool -> a)
  | SetDouble (Double -> a)
  | SetFloat (Float -> a)
  | SetInt (Int -> a)
  | SetSorry -- ^ not yet implemented

accessors :: Lookup a => a -> AccessorTree a
accessors x = toAccessorTree x id id
--accessors = flip (flip toAccessorTree id) id

showMsgs :: [String] -> String
showMsgs = intercalate "."

flatten :: AccessorTree a -> [(String, Getter a, Setter a)]
flatten = flatten' []

flatten' :: [String] -> AccessorTree a -> [(String, Getter a, Setter a)]
flatten' msgs (ATGetter (get, set)) = [(showMsgs (reverse msgs), get, set)]
flatten' msgs (Data (_,_) trees) = concatMap f trees
  where
    f (name,tree) = flatten' (name:msgs) tree

-- | Things which you can make a tree of labeled getters for.
-- You should derive this using GHC.Generics.
class Lookup a where
  toAccessorTree :: a -> (b -> a) -> (a -> b) -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a)) => a -> (b -> a) -> (a -> b) -> AccessorTree b
  toAccessorTree x get set = gtoAccessorTree (from x) (from . get) (set . to)

class GLookup f where
  gtoAccessorTree :: f a -> (b -> f a) -> (f a -> b) -> AccessorTree b

class GLookupS f where
  gtoAccessorTreeS :: f a -> (b -> f a) -> (f a -> b) -> [(String, AccessorTree b)]

-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toAccessorTree _ _ _ =
    Data ("V0", "V0") []
instance Lookup a => Lookup (Linear.V1 a) where
  toAccessorTree xyz get set =
    Data ("V1", "V1") [ ("x", toAccessorTree (getX xyz) (getX . get) (set . setX))
                      ]
    where
      getX (Linear.V1 x) = x
      setX x = Linear.V1 x
instance Lookup a => Lookup (Linear.V2 a) where
  toAccessorTree (Linear.V2 x0 y0) get set =
    Data ("V2", "V2") [ ("x", toAccessorTree x0 (getX . get) (set . setX))
                      , ("y", toAccessorTree y0 (getY . get) (set . setY))
                      ]
    where
      getX (Linear.V2 x _) = x
      getY (Linear.V2 _ y) = y
      setX x = Linear.V2 x  y0
      setY y = Linear.V2 x0 y
instance Lookup a => Lookup (Linear.V3 a) where
  toAccessorTree (Linear.V3 x0 y0 z0) get set =
    Data ("V3", "V3") [ ("x", toAccessorTree x0 (getX . get) (set . setX))
                      , ("y", toAccessorTree y0 (getY . get) (set . setY))
                      , ("z", toAccessorTree z0 (getZ . get) (set . setZ))
                      ]
    where
      getX (Linear.V3 x _ _) = x
      getY (Linear.V3 _ y _) = y
      getZ (Linear.V3 _ _ z) = z
      setX x = Linear.V3 x  y0 z0
      setY y = Linear.V3 x0 y  z0
      setZ z = Linear.V3 x0 y0 z
instance Lookup a => Lookup (Linear.V4 a) where
  toAccessorTree (Linear.V4 x0 y0 z0 w0) get set =
    Data ("V4", "V4") [ ("x", toAccessorTree x0 (getX . get) (set . setX))
                      , ("y", toAccessorTree y0 (getY . get) (set . setY))
                      , ("z", toAccessorTree z0 (getZ . get) (set . setZ))
                      , ("w", toAccessorTree w0 (getW . get) (set . setW))
                      ]
    where
      getX (Linear.V4 x _ _ _) = x
      getY (Linear.V4 _ y _ _) = y
      getZ (Linear.V4 _ _ z _) = z
      getW (Linear.V4 _ _ _ w) = w
      setX x = Linear.V4 x  y0 z0 w0
      setY y = Linear.V4 x0 y  z0 w0
      setZ z = Linear.V4 x0 y0 z  w0
      setW w = Linear.V4 x0 y0 z0 w
instance Lookup a => Lookup (Linear.Quaternion a) where
  toAccessorTree (Linear.Quaternion q0 (Linear.V3 x0 y0 z0)) get set =
    Data ("Quaternion", "Quaternion")
    [ ("q0", toAccessorTree q0 (getQ0 . get) (set . setQ0))
    , ("q1", toAccessorTree x0 (getQ1 . get) (set . setQ1))
    , ("q2", toAccessorTree y0 (getQ2 . get) (set . setQ2))
    , ("q3", toAccessorTree z0 (getQ3 . get) (set . setQ3))
    ]
    where
      getQ0 (Linear.Quaternion q _) = q
      getQ1 (Linear.Quaternion _ (Linear.V3 x _ _)) = x
      getQ2 (Linear.Quaternion _ (Linear.V3 _ y _)) = y
      getQ3 (Linear.Quaternion _ (Linear.V3 _ _ z)) = z

      setQ0 q = (Linear.Quaternion q  (Linear.V3 x0 y0 z0))
      setQ1 x = (Linear.Quaternion q0 (Linear.V3 x  y0 z0))
      setQ2 y = (Linear.Quaternion q0 (Linear.V3 x0 y  z0))
      setQ3 z = (Linear.Quaternion q0 (Linear.V3 x0 y0 z ))


instance Lookup f => GLookup (Rec0 f) where
  gtoAccessorTree x get set = toAccessorTree (unK1 x) (unK1 . get) (set . K1)

instance (Selector s, GLookup a) => GLookupS (S1 s a) where
  gtoAccessorTreeS x get set = [(selname, gtoAccessorTree (unM1 x) (unM1 . get) (set . M1))]
    where
      selname = case selName x of
        [] -> "()"
        y -> y

instance GLookupS U1 where
  gtoAccessorTreeS _ _ _ = []

instance (GLookupS f, GLookupS g) => GLookupS (f :*: g) where
  gtoAccessorTreeS (x :*: y) get set = tf ++ tg
    where
      tf = gtoAccessorTreeS x (getLeft  . get) (set . setLeft)
      tg = gtoAccessorTreeS y (getRight . get) (set . setRight)

      getLeft  (x' :*: _ ) = x'
      getRight (_  :*: y') = y'

      setLeft  x' = x' :*: y
      setRight y' = x  :*: y'

instance (Datatype d, Constructor c, GLookupS a) => GLookup (D1 d (C1 c a)) where
  gtoAccessorTree d@(M1 c) get set = Data (datatypeName d, conName c) con
    where
      con = gtoAccessorTreeS (unM1 c) (unM1 . unM1 . get) (set . M1 . M1)

-- basic types
instance Lookup () where -- hack to get dummy tree
  toAccessorTree _ _ _ = ATGetter (GetSorry, SetSorry)
instance Lookup Int where
  toAccessorTree _ get set = ATGetter (GetInt get, SetInt set)
instance Lookup Float where
  toAccessorTree _ get set = ATGetter (GetFloat get, SetFloat set)
instance Lookup Double where
  toAccessorTree _ get set = ATGetter (GetDouble get, SetDouble set)
instance Lookup Bool where
  toAccessorTree _ get set = ATGetter (GetBool get, SetBool set)

-- Word types
instance Lookup Word where
  toAccessorTree _ get _ = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup Word8 where
  toAccessorTree _ get _ = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup Word16 where
  toAccessorTree _ get _ = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup Word32 where
  toAccessorTree _ get _ = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup Word64 where
  toAccessorTree _ get _ = ATGetter (GetDouble (realToFrac . get), SetSorry)

-- Int types
instance Lookup Int8 where
  toAccessorTree _ get _ = ATGetter (GetInt (fromIntegral . get), SetSorry)
instance Lookup Int16 where
  toAccessorTree _ get _ = ATGetter (GetInt (fromIntegral . get), SetSorry)
instance Lookup Int32 where
  toAccessorTree _ get _ = ATGetter (GetInt (fromIntegral . get), SetSorry)
instance Lookup Int64 where
  toAccessorTree _ get _ = ATGetter (GetInt (fromIntegral . get), SetSorry)

-- todo(greg): some of these getters can fit in ints
-- C types
instance Lookup CChar where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CSChar where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUChar where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CShort where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUShort where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CInt where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUInt where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CLong where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CULong where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CPtrdiff where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CSize where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CWchar where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CSigAtomic where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CLLong where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CULLong where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CIntPtr where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUIntPtr where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CIntMax where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUIntMax where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CClock where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CTime where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CUSeconds where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CSUSeconds where
  toAccessorTree _ get _set = ATGetter (GetDouble (realToFrac . get), SetSorry)
instance Lookup CFloat where
  toAccessorTree _ get set = ATGetter (GetFloat (realToFrac . get), SetFloat (set . realToFrac))
instance Lookup CDouble where
  toAccessorTree _ get set = ATGetter (GetDouble (realToFrac . get), SetDouble (set . realToFrac))

-- other types
instance Lookup a => Lookup (Rot f1 f2 a) where
  toAccessorTree x get set = toAccessorTree (unR x) (unR . get) (set . Rot)
instance Lookup a => Lookup (V3T f a) where
  toAccessorTree x get set = toAccessorTree (unV x) (unV . get) (set . V3T)
instance Lookup a => Lookup (Euler a)

showAccTrees :: (Getter a -> String) -> [(String, AccessorTree a)] -> String -> [String]
showAccTrees show' trees spaces = concat cs ++ [spaces ++ "}"]
  where
    cs = zipWith (showRecordField show' spaces) trees ("{ " : repeat ", ")

showRecordField :: (Getter a -> String) -> String -> (String, AccessorTree a) -> String -> [String]
showRecordField show' spaces (getterName, ATGetter (get, _)) prefix =
  [spaces ++ prefix ++ getterName ++ " = " ++ show' get]
showRecordField show' spaces (getterName, Data (_,cons) trees) prefix =
  (spaces ++ prefixNameEq ++ cons) : showAccTrees show' trees newSpaces
  where
    prefixNameEq = prefix ++ getterName ++ " = "
    newSpaces = spaces ++ (replicate (length prefixNameEq) ' ')

-- | Show a tree of values
showTree :: AccessorTree a -> (Getter a -> String) -> String
showTree (Data (_,cons) trees) show' = init $ unlines $ cons : showAccTrees show' trees ""
showTree (ATGetter (get,_)) show' = show' get

-- | Show a list of values
-- .
-- True --> align the colums, False --> total mayhem
showFlat :: forall a . AccessorTree a -> Bool -> (Getter a -> String) -> String
showFlat at align show' = init $ unlines $ map f fl
  where
    fst3 (z,_,_) = z
    n = maximum (map (length . fst3) fl)

    f (name, get, _) = name ++ spaces ++ " = " ++ show' get
      where
        spaces
          | align = replicate (n - length name) ' '
          | otherwise = ""

    fl :: [(String, Getter a, Setter a)]
    fl = flatten at
