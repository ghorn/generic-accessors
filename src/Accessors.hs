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
       , flatten'
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
  SetBool (Bool -> a -> a)
  | SetDouble (Double -> a -> a)
  | SetFloat (Float -> a -> a)
  | SetInt (Int -> a -> a)
  | SetSorry -- ^ not yet implemented

accessors :: Lookup a => a -> AccessorTree a
accessors x = toAccessorTree x id (\new _ -> new)
--accessors = flip (flip toAccessorTree id) id

showMsgs :: [String] -> String
showMsgs = intercalate "."

flatten :: AccessorTree a -> [(String, Getter a, Setter a)]
flatten = map f . flatten'
  where
    f (x,y,z) = (showMsgs x, y, z)

flatten' :: AccessorTree a -> [([String], Getter a, Setter a)]
flatten' = flattenChain []
  where
    flattenChain :: [String] -> AccessorTree a -> [([String], Getter a, Setter a)]
    flattenChain msgs (ATGetter (get, set)) = [(reverse msgs, get, set)]
    flattenChain msgs (Data (_,_) trees) = concatMap f trees
      where
        f (name,tree) = flattenChain (name:msgs) tree

-- | Things which you can make a tree of labeled getters for.
-- You should derive this using GHC.Generics.
class Lookup a where
  toAccessorTree :: a -> (b -> a) -> (a -> b -> b) -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a)) => a -> (b -> a) -> (a -> b -> b) -> AccessorTree b
  toAccessorTree x get set = gtoAccessorTree (from x) (from . get) (set . to)

class GLookup f where
  gtoAccessorTree :: f a -> (b -> f a) -> (f a -> b -> b) -> AccessorTree b

class GLookupS f where
  gtoAccessorTreeS :: f a -> (b -> f a) -> (f a -> b -> b) -> [(String, AccessorTree b)]

-- some instance from linear
instance Lookup a => Lookup (Linear.V0 a) where
  toAccessorTree _ _ _ =
    Data ("V0", "V0") []
instance Lookup a => Lookup (Linear.V1 a) where
  toAccessorTree xyz get set =
    Data ("V1", "V1") [ ("x", toAccessorTree (getX xyz) (getX . get) setX)
                      ]
    where
      getX (Linear.V1 x) = x
      setX x new = set (Linear.V1 x) new
instance Lookup a => Lookup (Linear.V2 a) where
  toAccessorTree (Linear.V2 x0 y0) get set =
    Data ("V2", "V2") [ ("x", toAccessorTree x0 (getX . get) setX)
                      , ("y", toAccessorTree y0 (getY . get) setY)
                      ]
    where
      getX (Linear.V2 x _) = x
      getY (Linear.V2 _ y) = y

      setX x new = set (Linear.V2 x (getY (get new))) new
      setY y new = set (Linear.V2 (getX (get new)) y) new

instance Lookup a => Lookup (Linear.V3 a) where
  toAccessorTree (Linear.V3 x0 y0 z0) get set =
    Data ("V3", "V3") [ ("x", toAccessorTree x0 (getX . get) setX)
                      , ("y", toAccessorTree y0 (getY . get) setY)
                      , ("z", toAccessorTree z0 (getZ . get) setZ)
                      ]
    where
      getX (Linear.V3 x _ _) = x
      getY (Linear.V3 _ y _) = y
      getZ (Linear.V3 _ _ z) = z
      setX x new = set (Linear.V3 x y z) new
        where
          Linear.V3 _ y z = get new
      setY y new = set (Linear.V3 x y z) new
        where
          Linear.V3 x _ z = get new
      setZ z new = set (Linear.V3 x y z) new
        where
          Linear.V3 x y _ = get new
instance Lookup a => Lookup (Linear.V4 a) where
  toAccessorTree (Linear.V4 x0 y0 z0 w0) get set =
    Data ("V4", "V4") [ ("x", toAccessorTree x0 (getX . get) setX)
                      , ("y", toAccessorTree y0 (getY . get) setY)
                      , ("z", toAccessorTree z0 (getZ . get) setZ)
                      , ("w", toAccessorTree w0 (getW . get) setW)
                      ]
    where
      getX (Linear.V4 x _ _ _) = x
      getY (Linear.V4 _ y _ _) = y
      getZ (Linear.V4 _ _ z _) = z
      getW (Linear.V4 _ _ _ w) = w
      setX x new = set (Linear.V4 x y z w) new
        where
          Linear.V4 _ y z w = get new
      setY y new = set (Linear.V4 x y z w) new
        where
          Linear.V4 x _ z w = get new
      setZ z new = set (Linear.V4 x y z w) new
        where
          Linear.V4 x y _ w = get new
      setW w new = set (Linear.V4 x y z w) new
        where
          Linear.V4 x y z _ = get new
instance Lookup a => Lookup (Linear.Quaternion a) where
  toAccessorTree (Linear.Quaternion q0 (Linear.V3 x0 y0 z0)) get set =
    Data ("Quaternion", "Quaternion")
    [ ("q0", toAccessorTree q0 (getQ0 . get) setQ0)
    , ("q1", toAccessorTree x0 (getQ1 . get) setQ1)
    , ("q2", toAccessorTree y0 (getQ2 . get) setQ2)
    , ("q3", toAccessorTree z0 (getQ3 . get) setQ3)
    ]
    where
      getQ0 (Linear.Quaternion q _) = q
      getQ1 (Linear.Quaternion _ (Linear.V3 x _ _)) = x
      getQ2 (Linear.Quaternion _ (Linear.V3 _ y _)) = y
      getQ3 (Linear.Quaternion _ (Linear.V3 _ _ z)) = z

      setQ0 q new = set (Linear.Quaternion q (Linear.V3 x y z)) new
        where
          Linear.Quaternion _ (Linear.V3 x y z) = get new
      setQ1 x new = set (Linear.Quaternion q (Linear.V3 x y z)) new
        where
          Linear.Quaternion q (Linear.V3 _ y z) = get new
      setQ2 y new = set (Linear.Quaternion q (Linear.V3 x y z)) new
        where
          Linear.Quaternion q (Linear.V3 x _ z) = get new
      setQ3 z new = set (Linear.Quaternion q (Linear.V3 x y z)) new
        where
          Linear.Quaternion q (Linear.V3 x y _) = get new


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
  gtoAccessorTreeS (x0 :*: y0) get set = tf ++ tg
    where
      tf = gtoAccessorTreeS x0 (getLeft  . get) setLeft
      tg = gtoAccessorTreeS y0 (getRight . get) setRight

      getLeft  (x :*: _) = x
      getRight (_ :*: y) = y

      setLeft  x new = set (x :*: getRight (get new)) new
      setRight y new = set (getLeft (get new) :*: y) new

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

showAccTrees :: (Double -> String) -> a -> [(String, AccessorTree a)] -> String -> [String]
showAccTrees show' x trees spaces = concat cs ++ [spaces ++ "}"]
  where
    cs = zipWith (showRecordField show' x spaces) trees ("{ " : repeat ", ")

showVal :: Getter a -> (Double -> String) -> a -> String
showVal (GetBool get) _ x = show (get x)
showVal (GetInt get) _ x = show (get x)
showVal (GetDouble get) show' x = show' (get x)
showVal (GetFloat get) show' x = show' (realToFrac (get x))
showVal GetSorry _ _ = ""

showRecordField :: (Double -> String) -> a -> String -> (String, AccessorTree a) -> String -> [String]
showRecordField show' x spaces (getterName, ATGetter (get, _)) prefix =
  [spaces ++ prefix ++ getterName ++ " = " ++ showVal get show' x]
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
showTree (ATGetter (get,_)) show' x = showVal get show' x

-- | Show a list of values
-- .
-- True --> align the colums, False --> total mayhem
showFlat :: forall a . AccessorTree a -> Bool -> (Double -> String) -> a -> String
showFlat at align show' x = initUnlines $ map f fl
  where
    fst3 (z,_,_) = z
    n = maximum (map (length . fst3) fl)

    f (name, get, _) = name ++ spaces ++ " = " ++ showVal get show' x
      where
        spaces
          | align = replicate (n - length name) ' '
          | otherwise = ""

    fl :: [(String, Getter a, Setter a)]
    fl = flatten at
