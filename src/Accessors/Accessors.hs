{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Accessors.Accessors
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
       ) where

import GHC.Generics

import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Control.Lens ( Lens', Prism', (^.), (.~), preview, prism, withPrism )
import Data.List ( intercalate )
import Text.Printf ( printf )

type AccessorTree a = Either (GAField a) (GAData a)

data GAData a = GAData String (GAConstructor a)

data GAConstructor a =
  GAConstructor String [(Maybe String, AccessorTree a)]
  | GASum (GASimpleEnum a)

data GASimpleEnum a =
  GASimpleEnum
  { eConstructors :: [String]
  , eToString :: a -> String
  , eToIndex :: a -> Int
  , eFromString :: a -> String -> Either String a
  , eFromIndex :: a -> Int -> Either String a
  }

data GAField a =
  FieldDouble (Lens' a Double)
  | FieldFloat (Lens' a Float)
  | FieldInt8 (Lens' a Int8)
  | FieldInt16 (Lens' a Int16)
  | FieldInt32 (Lens' a Int32)
  | FieldInt64 (Lens' a Int64)
  | FieldWord8 (Lens' a Word8)
  | FieldWord16 (Lens' a Word16)
  | FieldWord32 (Lens' a Word32)
  | FieldWord64 (Lens' a Word64)
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
showGAConstructor spaces (GASum e) = [spaces ++ "[" ++ intercalate ", " (eConstructors e) ++ "]"]
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
describeGAField (FieldFloat _)  = "Float"
describeGAField (FieldInt8 _)   = "Int8"
describeGAField (FieldInt16 _)  = "Int16"
describeGAField (FieldInt32 _)  = "Int32"
describeGAField (FieldInt64 _)  = "Int64"
describeGAField (FieldWord8 _)  = "Word8"
describeGAField (FieldWord16 _) = "Word16"
describeGAField (FieldWord32 _) = "Word32"
describeGAField (FieldWord64 _) = "Word64"
describeGAField (FieldString _) = "String"
describeGAField FieldSorry      = "Sorry"

-- | Returns True if the __type__ of fields is the same.
sameFieldType :: GAField a -> GAField b -> Bool
sameFieldType (FieldDouble _) (FieldDouble _) = True
sameFieldType (FieldFloat _) (FieldFloat _)   = True
sameFieldType (FieldInt8 _) (FieldInt8 _)     = True
sameFieldType (FieldInt16 _) (FieldInt16 _)   = True
sameFieldType (FieldInt32 _) (FieldInt32 _)   = True
sameFieldType (FieldInt64 _) (FieldInt64 _)   = True
sameFieldType (FieldWord8 _) (FieldWord8 _)   = True
sameFieldType (FieldWord16 _) (FieldWord16 _) = True
sameFieldType (FieldWord32 _) (FieldWord32 _) = True
sameFieldType (FieldWord64 _) (FieldWord64 _) = True
sameFieldType (FieldString _) (FieldString _) = True
sameFieldType FieldSorry FieldSorry           = True
sameFieldType (FieldDouble _)  _              = False
sameFieldType (FieldFloat _)   _              = False
sameFieldType (FieldInt8 _)    _              = False
sameFieldType (FieldInt16 _)   _              = False
sameFieldType (FieldInt32 _)   _              = False
sameFieldType (FieldInt64 _)   _              = False
sameFieldType (FieldWord8 _)   _              = False
sameFieldType (FieldWord16 _)  _              = False
sameFieldType (FieldWord32 _)  _              = False
sameFieldType (FieldWord64 _)  _              = False
sameFieldType (FieldString _)  _              = False
sameFieldType FieldSorry       _              = False

accessors :: Lookup a => AccessorTree a
accessors = toAccessorTree id

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
  toAccessorTree :: Lens' b a -> AccessorTree b

  default toAccessorTree :: (Generic a, GLookup (Rep a)) => Lens' b a -> AccessorTree b
  toAccessorTree lens0 = gtoAccessorTree (lens0 . repLens)
    where
      repLens :: Lens' a (Rep a p)
      repLens f y = fmap to (f (from y))

class GLookup f where
  gtoAccessorTree :: Lens' b (f a) -> AccessorTree b

class GLookupS f where
  gtoAccessorTreeS :: Lens' b (f a) -> [(Maybe String, AccessorTree b)]

instance Lookup f => GLookup (Rec0 f) where
  gtoAccessorTree :: Lens' b (Rec0 f p) -> AccessorTree b
  gtoAccessorTree lens0 = toAccessorTree (lens0 . rec0Lens)
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

instance (Datatype d, GEnum (c1 :+: c2)) => GLookup (D1 d (c1 :+: c2)) where
  gtoAccessorTree :: forall b p . Lens' b (D1 d (c1 :+: c2) p) -> AccessorTree b
  gtoAccessorTree lens0 = Right $ GAData (datatypeName datatypeError) constructor
    where
      datatypeError :: D1 d (c1 :+: c2) p
      datatypeError = error $ "generic-accessors: datatypeName should never access data"

      constructor :: GAConstructor b
      constructor =
        GASum
        GASimpleEnum
        { eConstructors = options
        , eToString = toString
        , eToIndex = toIndex
        , eFromIndex = fromIndex
        , eFromString = fromString
        }
        where
          options = map fst3 simpleEnums
            where
              fst3 (x,_,_) = x

          fromIndex x k
            | k < 0 =
                Left $
                printf "generic-accessors: Error converting Int to Enum: requested negative index (%d)" k
            | k >= length options =
                Left $
                "generic-accessors: Error converting Int to Enum.\n" ++
                printf "Requested index %d but there are only %d options." k (length simpleEnums)
            | otherwise = Right $ (lens0 . m1Lens . intLens .~ k) x

          fromString x name = fromString' 0 options
            where
              fromString' k (opt:opts)
                | name == opt = fromIndex x k
                | otherwise = fromString' (k+1) opts
              fromString' _ [] =
                Left $
                "generic-accessors: Error converting from String to Enum: "
                ++ show name ++ "is not one of the constructors."

          toIndex x = x ^. (lens0 . m1Lens . intLens)
          toString x = case safeIndex options index of
            Just r -> r
            Nothing -> error $ unlines
                       [ "generic-accessors: eToString: the \"impossible\" happened"
                       , printf "Enum is out of bounds (index %d, length %d)." index (length options)
                       ]
            where
              index = toIndex x

      simpleEnums :: [(String, (c1 :+: c2) p, (c1 :+: c2) p -> Bool)]
      simpleEnums = gtoSimpleEnum id

      intLens :: Lens' ((c1 :+: c2) p) Int
      intLens f y = fmap fromInt' (f (toInt y))

      fromInt' :: Int -> (c1 :+: c2) p
      fromInt' k = case fromInt k of
        Right r -> r
        Left e -> error e

      fromInt :: Int -> Either String ((c1 :+: c2) p)
      fromInt k = case safeIndex simpleEnums k of
        Nothing -> Left $
                   "generic-accessors:\n" ++
                   "Error converting Int to Enum.\n" ++
                   printf "Requested index %d but there are only %d options." k (length simpleEnums)
        Just (_, x, _) -> Right x

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
  gtoAccessorTree :: forall b p . Lens' b (D1 d (C1 c a) p) -> AccessorTree b
  gtoAccessorTree lens0 = Right $ GAData (datatypeName datatypeError) constructor
    where
      datatypeError :: D1 d (C1 c a) p
      datatypeError = error $ "generic-accessors: datatypeName should never access data"

      constructor :: GAConstructor b
      constructor = gtoAccessorTreeC (lens0 . m1Lens)

gtoAccessorTreeC :: forall c b a p . (Constructor c, GLookupS a) => Lens' b (C1 c a p) -> GAConstructor b
gtoAccessorTreeC lens0 = GAConstructor (conName conError) (gtoAccessorTreeS (lens0 . m1Lens))
  where
    conError :: C1 c a p
    conError = error $ "generic-accessors: conName should never access data"

instance (Selector s, GLookup a) => GLookupS (S1 s a) where
  gtoAccessorTreeS :: Lens' b (S1 s a p) -> [(Maybe String, AccessorTree b)]
  gtoAccessorTreeS lens0 = [(selname, gtoAccessorTree (lens0 . m1Lens))]
    where
      selname = case selName selError of
        "" -> Nothing
        y -> Just y

      selError :: S1 s a p
      selError = error $ "generic-accessors: selName should never access data"

instance GLookupS U1 where
  gtoAccessorTreeS :: Lens' b (U1 p) -> [(Maybe String, AccessorTree b)]
  gtoAccessorTreeS _ = []

instance (GLookupS f, GLookupS g) => GLookupS (f :*: g) where
  gtoAccessorTreeS :: Lens' b ((f :*: g) p) -> [(Maybe String, AccessorTree b)]
  gtoAccessorTreeS lens0 = tf ++ tg
    where
      tf = gtoAccessorTreeS (lens0 . leftLens)
      tg = gtoAccessorTreeS (lens0 . rightLens)

      leftLens ::  Lens' ((f :*: g) a) (f a)
      leftLens  f (x :*: y) = fmap (\x' -> x' :*: y ) (f x)

      rightLens :: Lens' ((f :*: g) a) (g a)
      rightLens f (x :*: y) = fmap (\y' -> x  :*: y') (f y)


showAccTrees :: (Double -> String) -> a
                -> [(Maybe String, AccessorTree a)] -> String
                -> [String]
showAccTrees show' x trees spaces = concat cs ++ [spaces ++ "}"]
  where
    cs = zipWith (showRecordField show' x spaces) trees ("{ " : repeat ", ")

showFieldVal :: GAField a -> (Double -> String) -> a -> String
showFieldVal (FieldInt8 lens) _ x = show (x ^. lens)
showFieldVal (FieldInt16 lens) _ x = show (x ^. lens)
showFieldVal (FieldInt32 lens) _ x = show (x ^. lens)
showFieldVal (FieldInt64 lens) _ x = show (x ^. lens)
showFieldVal (FieldWord8 lens) _ x = show (x ^. lens)
showFieldVal (FieldWord16 lens) _ x = show (x ^. lens)
showFieldVal (FieldWord32 lens) _ x = show (x ^. lens)
showFieldVal (FieldWord64 lens) _ x = show (x ^. lens)
showFieldVal (FieldDouble lens) show' x = show' (x ^. lens)
showFieldVal (FieldFloat lens) show' x = show' (realToFrac (x ^. lens))
showFieldVal (FieldString lens) _ x = x ^. lens
showFieldVal FieldSorry _ _ = ""

showTipVal :: GATip a -> (Double -> String) -> a -> String
showTipVal (GATipField f) sh x = showFieldVal f sh x
showTipVal (GATipSimpleEnum simpleEnum) _ x = eToString simpleEnum x

showRecordField :: (Double -> String) -> a -> String -> (Maybe String, AccessorTree a) -> String -> [String]
showRecordField show' x spaces (getterName, (Left field)) prefix =
  [spaces ++ prefix ++ showMName getterName ++ " = " ++ showFieldVal field show' x]
showRecordField _ x spaces (getterName, Right (GAData _ (GASum simpleEnum))) prefix =
  [spaces ++ prefix ++ showMName getterName ++ " = " ++ eToString simpleEnum x]
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
showTree (Right (GAData _ (GASum simpleEnum))) _ x = eToString simpleEnum x
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
