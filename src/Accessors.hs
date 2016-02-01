{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

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

import GHC.Generics

import Control.Lens ( Lens', Prism', (^.), preview, prism, withPrism )
import Data.List ( intercalate )
import qualified Linear
import GHC.Word
import Data.Int
import Foreign.C.Types
import Text.Printf ( printf )

import SpatialMath ( Euler )
import SpatialMathT ( V3T(..), Rot(..) )

type AccessorTree a = Either (GAField a) (GAData a)

data GAData a = GAData String (GAConstructor a)

data GAConstructor a =
  GAConstructor String [(Maybe String, AccessorTree a)]
  | GASum (GASimpleEnum a)

data GASimpleEnum a = GASimpleEnum [String] (Lens' a Int)

data GAField a =
  FieldDouble (Lens' a Double)
  | FieldFloat (Lens' a Float)
  | FieldInt (Lens' a Int)
  | FieldString (Lens' a String)
  | FieldSorry -- ^ a field which is not yet supported

data GATip a =
  GATipSimpleEnum (GASimpleEnum a)
  | GATipField (GAField a)

showGAData :: String -> GAData a -> [String]
showGAData spaces (GAData name constructors) =
  (spaces ++ "Data " ++ show name) :
  showGAConstructor (spaces ++ "    ") constructors

showGAConstructor :: String -> GAConstructor a -> [String]
showGAConstructor spaces (GASum (GASimpleEnum names _)) = [spaces ++ "[" ++ intercalate ", " names ++ "]"]
showGAConstructor spaces (GAConstructor name fields) =
  (spaces ++ name) : concatMap (showGAField (spaces ++ "    ")) fields

showGAField :: String -> (Maybe String, AccessorTree a) -> [String]
showGAField spaces (name, Left f) = [spaces ++ showMName name ++ " " ++ describeGAField f]
showGAField spaces (name, Right field) =
  showMName name :
  showGAData (spaces ++ "    ") field

showMName :: Maybe String -> String
showMName (Just n) = n
showMName Nothing = "()"

instance Show (GAData a) where
  show = unlines . showGAData ""

-- | Return the type of field, such as "Bool", "Double", "String", etc.
describeGAField :: GAField a -> String
describeGAField (FieldDouble _) = "Double"
describeGAField (FieldFloat _) = "Float"
describeGAField (FieldInt _) = "Int"
describeGAField (FieldString _) = "String"
describeGAField FieldSorry = "Sorry"

-- | Returns True if the __type__ of fields is the same.
sameFieldType :: GAField a -> GAField b -> Bool
sameFieldType (FieldDouble _) (FieldDouble _) = True
sameFieldType (FieldFloat _) (FieldFloat _) = True
sameFieldType (FieldInt _) (FieldInt _) = True
sameFieldType (FieldString _) (FieldString _) = True
sameFieldType FieldSorry FieldSorry = True
sameFieldType (FieldDouble _) _ = False
sameFieldType (FieldFloat _) _ = False
sameFieldType (FieldInt _) _ = False
sameFieldType (FieldString _) _ = False
sameFieldType FieldSorry _ = False

accessors :: Lookup a => AccessorTree a
accessors = toGAData id

showMsgs :: [Maybe String] -> String
showMsgs = intercalate "." . map showMName

flatten :: AccessorTree a -> [(String, GATip a)]
flatten = map f . flatten'
  where
    f (x,y) = (showMsgs x, y)

flatten' :: AccessorTree a -> [([Maybe String], GATip a)]
flatten' = flattenChain []
  where
    flattenChain :: [Maybe String] -> AccessorTree a -> [([Maybe String], GATip a)]
    flattenChain msgs (Left f) = [(reverse msgs, GATipField f)]
    flattenChain msgs (Right (GAData _ (GASum simpleEnum))) = [(reverse msgs, GATipSimpleEnum simpleEnum)]
    flattenChain msgs (Right (GAData _ (GAConstructor _ trees))) = concatMap f trees
      where
        f :: (Maybe String, AccessorTree a) -> [([Maybe String], GATip a)]
        f (name, tree) = flattenChain (name:msgs) tree

-- | Things which you can make a tree of labeled getters for.
-- You should derive this using GHC.Generics.
class Lookup a where
  toGAData :: Lens' b a -> AccessorTree b

  default toGAData :: (Generic a, GLookup (Rep a)) => Lens' b a -> AccessorTree b
  toGAData lens0 = gtoGAData (lens0 . repLens)
    where
      repLens :: Lens' a (Rep a p)
      repLens f y = fmap to (f (from y))

class GLookup f where
  gtoGAData :: Lens' b (f a) -> AccessorTree b

class GLookupS f where
  gtoGADataS :: Lens' b (f a) -> [(Maybe String, AccessorTree b)]

instance Lookup f => GLookup (Rec0 f) where
  gtoGAData :: Lens' b (Rec0 f p) -> AccessorTree b
  gtoGAData lens0 = toGAData (lens0 . rec0Lens)
    where
      rec0Lens :: Lens' (Rec0 f a) f
      rec0Lens f y = fmap K1 (f (unK1 y))

class GEnum a where
  gtoSimpleEnum :: Prism' b (a p) -> [(String, b, b -> Bool)]

instance Constructor c => GEnum (C1 c U1) where
  gtoSimpleEnum :: forall b p . Prism' b (C1 c U1 p) -> [(String, b, b -> Bool)]
  gtoSimpleEnum pr = [(cname, thisOne, isThisOne)]
    where
      thisOne :: b
      thisOne = withPrism pr $ \f _ -> f (M1 U1 :: C1 c U1 p)

      isThisOne :: b -> Bool
      isThisOne b = case preview pr b :: Maybe (C1 c U1 p) of
        Nothing -> False
        Just _ -> True

      cname = conName conError

      conError :: C1 c a p
      conError = error $ "generic-accessors: conName should never access data"

instance (GEnum c1, GEnum c2) => GEnum (c1 :+: c2) where
  gtoSimpleEnum :: forall b p . Prism' b ((c1 :+: c2) p) -> [(String, b, b -> Bool)]
  gtoSimpleEnum pr0 = c1s ++ c2s
    where
      c1s = gtoSimpleEnum ((pr0 . leftPrism) :: Prism' b (c1 p))
      c2s = gtoSimpleEnum ((pr0 . rightPrism) :: Prism' b (c2 p))

      leftPrism :: Prism' ((c1 :+: c2) p) (c1 p)
      leftPrism = prism remitter reviewer
        where
          remitter = L1
          reviewer (L1 l) = Right l
          reviewer x = Left x

      rightPrism :: Prism' ((c1 :+: c2) p) (c2 p)
      rightPrism = prism remitter reviewer
        where
          remitter = R1
          reviewer (R1 l) = Right l
          reviewer x = Left x

instance (Datatype d, GEnum (c1 :+: c2), GEnum c1, GEnum c2) => GLookup (D1 d (c1 :+: c2)) where
  gtoGAData :: forall b p . Lens' b (D1 d (c1 :+: c2) p) -> AccessorTree b
  gtoGAData lens0 = Right $ GAData (datatypeName datatypeError) constructor
    where
      datatypeError :: D1 d (c1 :+: c2) p
      datatypeError = error $ "generic-accessors: datatypeName should never access data"

      constructor :: GAConstructor b
      constructor = GASum $ GASimpleEnum (map fst3 simpleEnums) (lens0 . m1Lens . intLens)
        where
          fst3 (x,_,_) = x

      simpleEnums :: [(String, (c1 :+: c2) p, (c1 :+: c2) p -> Bool)]
      simpleEnums = gtoSimpleEnum id

      intLens :: Lens' ((c1 :+: c2) p) Int
      intLens f y = fmap fromInt (f (toInt y))

      fromInt :: Int -> (c1 :+: c2) p
      fromInt k = case safeIndex simpleEnums k of
        Nothing -> error $ unlines
                   [ "generic-accessors:"
                   , "Error converting Int to Enum."
                   , printf "Requested index %d but there are only %d options" k (length simpleEnums)
                   ]
        Just (_, x, _) -> x

      toInt :: (c1 :+: c2) p -> Int
      toInt x = toInt' 0 simpleEnums
        where
          toInt' k ((_, _, isVal):others)
            | isVal x = k
            | otherwise = toInt' (k+1) others
          toInt' _ [] =
            error $ unlines
            [ "generic-accessors:"
            , "The \"impossible\" happened converting Enum to Int."
            , "No enum matched the provided one."
            ]

safeIndex :: [a] -> Int -> Maybe a
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) k = safeIndex xs (k-1)
safeIndex [] _ = Nothing

m1Lens :: Lens' (M1 i c f p) (f p)
m1Lens f y = fmap M1 (f (unM1 y))

instance (Datatype d, Constructor c, GLookupS a) => GLookup (D1 d (C1 c a)) where
  gtoGAData :: forall b p . Lens' b (D1 d (C1 c a) p) -> AccessorTree b
  gtoGAData lens0 = Right $ GAData (datatypeName datatypeError) constructor
    where
      datatypeError :: D1 d (C1 c a) p
      datatypeError = error $ "generic-accessors: datatypeName should never access data"

      constructor :: GAConstructor b
      constructor = gtoGADataC (lens0 . m1Lens)

gtoGADataC :: forall c b a p . (Constructor c, GLookupS a) => Lens' b (C1 c a p) -> GAConstructor b
gtoGADataC lens0 = GAConstructor (conName conError) (gtoGADataS (lens0 . m1Lens))
  where
    conError :: C1 c a p
    conError = error $ "generic-accessors: conName should never access data"

instance (Selector s, GLookup a) => GLookupS (S1 s a) where
  gtoGADataS :: Lens' b (S1 s a p) -> [(Maybe String, AccessorTree b)]
  gtoGADataS lens0 = [(selname, gtoGAData (lens0 . m1Lens))]
    where
      selname = case selName selError of
        "" -> Nothing
        y -> Just y

      selError :: S1 s a p
      selError = error $ "generic-accessors: selName should never access data"

instance GLookupS U1 where
  gtoGADataS :: Lens' b (U1 p) -> [(Maybe String, AccessorTree b)]
  gtoGADataS _ = []

instance (GLookupS f, GLookupS g) => GLookupS (f :*: g) where
  gtoGADataS :: Lens' b ((f :*: g) p) -> [(Maybe String, AccessorTree b)]
  gtoGADataS lens0 = tf ++ tg
    where
      tf = gtoGADataS (lens0 . leftLens)
      tg = gtoGADataS (lens0 . rightLens)

      leftLens ::  Lens' ((f :*: g) a) (f a)
      leftLens  f (x :*: y) = fmap (\x' -> x' :*: y ) (f x)

      rightLens :: Lens' ((f :*: g) a) (g a)
      rightLens f (x :*: y) = fmap (\y' -> x  :*: y') (f y)


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

showAccTrees :: (Double -> String) -> a
                -> [(Maybe String, AccessorTree a)] -> String
                -> [String]
showAccTrees show' x trees spaces = concat cs ++ [spaces ++ "}"]
  where
    cs = zipWith (showRecordField show' x spaces) trees ("{ " : repeat ", ")

showFieldVal :: GAField a -> (Double -> String) -> a -> String
showFieldVal (FieldInt lens) _ x = show (x ^. lens)
showFieldVal (FieldDouble lens) show' x = show' (x ^. lens)
showFieldVal (FieldFloat lens) show' x = show' (realToFrac (x ^. lens))
showFieldVal (FieldString lens) _ x = x ^. lens
showFieldVal FieldSorry _ _ = ""

showSimpleEnum :: a -> GASimpleEnum a -> String
showSimpleEnum x (GASimpleEnum names intLens) = case safeIndex names index of
  Just r -> r
  Nothing -> error $ unlines
             [ "generic-accessors: showSimpleEnum: the \"impossible\" happened"
             , printf "Enum is out of bounds (index %d, length %d)." index (length names)
             ]
  where
    index = x ^. intLens

showTipVal :: GATip a -> (Double -> String) -> a -> String
showTipVal (GATipField f) sh x = showFieldVal f sh x
showTipVal (GATipSimpleEnum simpleEnum) _ x = showSimpleEnum x simpleEnum

showRecordField :: (Double -> String) -> a -> String -> (Maybe String, AccessorTree a) -> String -> [String]
showRecordField show' x spaces (getterName, (Left field)) prefix =
  [spaces ++ prefix ++ showMName getterName ++ " = " ++ showFieldVal field show' x]
showRecordField _ x spaces (getterName, Right (GAData _ (GASum simpleEnum))) prefix =
  [spaces ++ prefix ++ showMName getterName ++ " = " ++ showSimpleEnum x simpleEnum]
showRecordField show' x spaces (getterName, Right (GAData _ (GAConstructor cons trees))) prefix =
  (spaces ++ prefixNameEq ++ cons) : showAccTrees show' x trees newSpaces
  where
    prefixNameEq = prefix ++ showMName getterName ++ " = "
    newSpaces = spaces ++ (replicate (length prefixNameEq) ' ')

-- version of (init . unlines) which doesn't throw an error on empty input
initUnlines :: [String] -> [Char]
initUnlines [] = ""
initUnlines xs = init (unlines xs)

-- | Show a tree of values
showTree :: AccessorTree a -> (Double -> String) -> a -> String
showTree (Right (GAData _ (GASum simpleEnum))) _ x = showSimpleEnum x simpleEnum
showTree (Right (GAData _ (GAConstructor cons trees))) show' x =
  initUnlines $ cons : showAccTrees show' x trees ""
showTree (Left field) show' x = showFieldVal field show' x

-- | Show a list of values
-- .
-- True --> align the colums, False --> total mayhem
showFlat :: forall a . AccessorTree a -> Bool -> (Double -> String) -> a -> String
showFlat at align show' x = initUnlines $ map f fl
  where
    n = maximum (map (length . fst) fl)

    f (name, lens) = name ++ spaces ++ " = " ++ showTipVal lens show' x
      where
        spaces
          | align = replicate (n - length name) ' '
          | otherwise = ""

    fl :: [(String, GATip a)]
    fl = flatten at
