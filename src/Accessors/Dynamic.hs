{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Accessors.Dynamic
       ( DTree, DData(..), DConstructor(..), DSimpleEnum(..), DField(..)
       , toDData, updateLookupable, describeDField, sameDFieldType
       , diffDTrees
         -- * some utility functions for working with DSimpleEnums
       , denumToString, denumToStringOrMsg, denumSetString, denumSetIndex
       ) where

import GHC.Generics

import Data.Binary ( Binary )
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Serialize ( Serialize )
import Data.Data ( Data )
import Data.Either ( partitionEithers )
import Data.List ( intercalate )
import Data.Typeable ( Typeable )
import Control.Lens
import Text.Printf ( printf )

import Accessors

type DTree = Either DField DData

data DData = DData String DConstructor
           deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Serialize DData
instance Binary DData

data DConstructor =
  DConstructor String [(Maybe String, DTree)]
  | DSum DSimpleEnum
  deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Serialize DConstructor
instance Binary DConstructor

data DSimpleEnum = DSimpleEnum [String] Int
                 deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Serialize DSimpleEnum
instance Binary DSimpleEnum

-- | a dynamic field
data DField =
  DDouble Double
  | DFloat Float
  | DInt8 Int8
  | DInt16 Int16
  | DInt32 Int32
  | DInt64 Int64
  | DWord8 Word8
  | DWord16 Word16
  | DWord32 Word32
  | DWord64 Word64
  | DString String
  | DSorry
  deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Serialize DField
instance Binary DField

-- | Get the constructor string or an error message.
denumToString :: DSimpleEnum -> Either String String
denumToString (DSimpleEnum _ k)
  | k < 0 = Left $ printf "denumToString: index %d is negative" k
denumToString (DSimpleEnum constructors k) = safeIndex constructors k
  where
    safeIndex (x:_) 0 = Right x
    safeIndex (_:xs) j = safeIndex xs (j-1)
    safeIndex [] _ =
      Left $
      printf "denumToString: index %d is too large (%d constructors)" k (length constructors)

-- | Get the constructor string or an error message without telling which is which.
denumToStringOrMsg :: DSimpleEnum -> String
denumToStringOrMsg d = case denumToString d of
  Left msg -> msg
  Right r -> r

-- | Try to update an enum with its constructor. Fail if not a valid constructor.
denumSetString :: DSimpleEnum -> String -> Either String DSimpleEnum
denumSetString (DSimpleEnum options _) txt = safeLookup options 0
  where
    safeLookup (opt:opts) k
      | opt == txt = Right (DSimpleEnum options k)
      | otherwise = safeLookup opts (k + 1)
    safeLookup [] _ = Left $ printf "denumSetString: %s is not a valid constructor" txt

-- | Try to update an enum with its index. Fail if out of bounds.
denumSetIndex :: DSimpleEnum -> Int -> Either String DSimpleEnum
denumSetIndex (DSimpleEnum constructors _) k
  | k < 0 = Left $ printf "denumSetIndex: index %d is negative" k
  | k >= length constructors =
      Left $
      printf "denumSetIndex: index %d is too large (%d constructors)" k (length constructors)
  | otherwise = Right $ DSimpleEnum constructors k

-- | Returns True if the __type__ of fields is the same.
sameDFieldType :: DField -> DField -> Bool
sameDFieldType (DDouble _) (DDouble _) = True
sameDFieldType (DFloat _) (DFloat _)   = True
sameDFieldType (DInt8 _) (DInt8 _)     = True
sameDFieldType (DInt16 _) (DInt16 _)   = True
sameDFieldType (DInt32 _) (DInt32 _)   = True
sameDFieldType (DInt64 _) (DInt64 _)   = True
sameDFieldType (DWord8 _) (DWord8 _)   = True
sameDFieldType (DWord16 _) (DWord16 _) = True
sameDFieldType (DWord32 _) (DWord32 _) = True
sameDFieldType (DWord64 _) (DWord64 _) = True
sameDFieldType (DString _) (DString _) = True
sameDFieldType DSorry DSorry           = True
sameDFieldType (DDouble _) _           = False
sameDFieldType (DFloat _)  _           = False
sameDFieldType (DInt8 _)   _           = False
sameDFieldType (DInt16 _)  _           = False
sameDFieldType (DInt32 _)  _           = False
sameDFieldType (DInt64 _)  _           = False
sameDFieldType (DWord8 _)  _           = False
sameDFieldType (DWord16 _) _           = False
sameDFieldType (DWord32 _) _           = False
sameDFieldType (DWord64 _) _           = False
sameDFieldType (DString _) _           = False
sameDFieldType DSorry      _           = False

describeDField :: DField -> String
describeDField (DInt8 _)   = "Int8"
describeDField (DInt16 _)  = "Int16"
describeDField (DInt32 _)  = "Int32"
describeDField (DInt64 _)  = "Int64"
describeDField (DWord8 _)  = "Word8"
describeDField (DWord16 _) = "Word16"
describeDField (DWord32 _) = "Word32"
describeDField (DWord64 _) = "Word64"
describeDField (DDouble _) = "Double"
describeDField (DFloat _)  = "Float"
describeDField (DString _) = "String"
describeDField DSorry      = "Sorry"

-- | convert to a dynamic value
toDData :: forall a . Lookup a => a -> DTree
toDData x = toDData' accessors
  where
    toDData' :: Either (GAField a) (GAData a) -> DTree
    toDData' (Right (GAData dname constructor)) =
      Right $ DData dname (toDConstructor constructor)
    toDData' (Left field) = Left (toDField field)

    toDConstructor :: GAConstructor a -> DConstructor
    toDConstructor (GASum e) =
      DSum (DSimpleEnum (eConstructors e) (eToIndex e x))

    toDConstructor (GAConstructor cname fields) =
      DConstructor cname $ map (\(n, f) -> (n, toDData' f)) fields

    toDField :: GAField a -> DField
    toDField (FieldDouble f) = DDouble (x ^. f)
    toDField (FieldFloat f)  = DFloat (x ^. f)
    toDField (FieldInt8 f)   = DInt8 (x ^. f)
    toDField (FieldInt16 f)  = DInt16 (x ^. f)
    toDField (FieldInt32 f)  = DInt32 (x ^. f)
    toDField (FieldInt64 f)  = DInt64 (x ^. f)
    toDField (FieldWord8 f)  = DWord8 (x ^. f)
    toDField (FieldWord16 f) = DWord16 (x ^. f)
    toDField (FieldWord32 f) = DWord32 (x ^. f)
    toDField (FieldWord64 f) = DWord64 (x ^. f)
    toDField (FieldString f) = DString (x ^. f)
    toDField FieldSorry      = DSorry


-- | Update something using a dynamic representation
updateLookupable :: Lookup a => a -> DTree -> Either String a
updateLookupable x0 dtree = updateData x0 accessors dtree

updateData :: forall a
                     . a
                     -> Either (GAField a) (GAData a)
                     -> DTree
                     -> Either String a
updateData x0 (Left afield) (Left dfield) = updateField x0 afield dfield
updateData x0 (Right (GAData adataName acon)) (Right (DData ddataName dcon))
  | adataName /= ddataName =
      Left $
      "dynamic datatype name " ++ show ddataName ++
      " don't match accessor datatype names " ++ show adataName
  | otherwise = updateConstructor x0 acon dcon
updateData _ (Left field) (Right (DData n _)) =
  Left $ "got GAField (" ++ describeGAField field ++ ") for accessor tree but DData (" ++ show n ++ ") for dynamic tree"
updateData _ (Right (GAData n _)) (Left field) =
  Left $ "got GAData for accessor tree (" ++ show n ++ ") but DField (" ++ describeDField field++ ") for dynamic tree"

showList' :: [String] -> String
showList' xs = "[" ++ intercalate ", " xs ++ "]"

updateConstructor :: forall a
                     . a
                     -> GAConstructor a
                     -> DConstructor
                     -> Either String a
updateConstructor x (GASum aenum) (DSum (DSimpleEnum dnames k))
  | anames /= dnames =
      Left $
      "accessor sum options " ++ showList' anames ++
      " doesn't match dynamic sum options " ++ showList' dnames
  | otherwise = eFromIndex aenum x k
  where
    anames = eConstructors aenum
updateConstructor x0 (GAConstructor aconName afields) (DConstructor dconName dfields)
  | aconName /= dconName =
      Left $
      "dynamic constructor name " ++ show dconName ++
      " don't match accessor constructor names " ++ show aconName
  | length afields /= length dfields = lengthMismatch
  | otherwise = f x0 afields dfields
    where
      lengthMismatch =
        Left $
        "dynamic fields have different length than accessor fields\n" ++
        "dynamic fields: " ++ show (map fst dfields) ++ "\n" ++
        "accessor fields: " ++ show (map fst afields)

      f :: a
           -> [(Maybe String, Either (GAField a) (GAData a))]
           -> [(Maybe String, DTree)]
           -> Either String a
      f x ((aname, afield):as) ((dname, dfield):ds)
        | aname /= dname =
            Left $
            "accessor selector name " ++ show aname ++
            " doesn't match dynamic selector name " ++ show dname
        | otherwise = case updateData x afield dfield of
            Left msg -> Left $ "error updating selector " ++ show aname ++ ": " ++ msg
            Right r -> f r as ds
      f x [] [] = Right x
      -- this should never happen:
      f _ _ _ = lengthMismatch
updateConstructor _ (GAConstructor aconName _) (DSum (DSimpleEnum dnames _)) =
  Left $ "got GAConstructor (" ++ aconName ++ ") but DSum ([" ++ showList' dnames ++ "])"
updateConstructor _ (GASum aenum) (DConstructor dconName _) =
  Left $ "got GASum ([" ++ showList' (eConstructors aenum) ++ "]) but DConstructor (" ++ dconName ++ ")"


updateField :: a -> GAField a -> DField -> Either String a
updateField x0 (FieldDouble f) (DDouble x) = Right $ (f .~ x) x0
updateField x0 (FieldFloat f) (DFloat x)   = Right $ (f .~ x) x0
updateField x0 (FieldInt8 f) (DInt8 x)     = Right $ (f .~ x) x0
updateField x0 (FieldInt16 f) (DInt16 x)   = Right $ (f .~ x) x0
updateField x0 (FieldInt32 f) (DInt32 x)   = Right $ (f .~ x) x0
updateField x0 (FieldInt64 f) (DInt64 x)   = Right $ (f .~ x) x0
updateField x0 (FieldWord8 f) (DWord8 x)   = Right $ (f .~ x) x0
updateField x0 (FieldWord16 f) (DWord16 x) = Right $ (f .~ x) x0
updateField x0 (FieldWord32 f) (DWord32 x) = Right $ (f .~ x) x0
updateField x0 (FieldWord64 f) (DWord64 x) = Right $ (f .~ x) x0
updateField x0 (FieldString f) (DString x) = Right $ (f .~ x) x0
updateField x0 FieldSorry _                = Right x0
updateField _ f@(FieldDouble _) d          = Left (fieldMismatch f d)
updateField _ f@(FieldFloat _)  d          = Left (fieldMismatch f d)
updateField _ f@(FieldInt8 _)   d          = Left (fieldMismatch f d)
updateField _ f@(FieldInt16 _)  d          = Left (fieldMismatch f d)
updateField _ f@(FieldInt32 _)  d          = Left (fieldMismatch f d)
updateField _ f@(FieldInt64 _)  d          = Left (fieldMismatch f d)
updateField _ f@(FieldWord8 _)  d          = Left (fieldMismatch f d)
updateField _ f@(FieldWord16 _) d          = Left (fieldMismatch f d)
updateField _ f@(FieldWord32 _) d          = Left (fieldMismatch f d)
updateField _ f@(FieldWord64 _) d          = Left (fieldMismatch f d)
updateField _ f@(FieldString _) d          = Left (fieldMismatch f d)

fieldMismatch :: GAField a -> DField -> String
fieldMismatch f d =
  "accessor GAField " ++ describeGAField f ++
  " got incompatible dynamic DField " ++ describeDField d


diffDTrees :: String -> DTree -> DTree -> [String]
diffDTrees rootName = diffDTrees' [rootName]

showName :: [String] -> String
showName = intercalate "." . reverse

diffDTrees' :: [String] -> DTree -> DTree -> [String]
diffDTrees' name (Left x) (Left y) = case diffDFields name x y of
  Nothing -> []
  Just r -> [r]
diffDTrees' name (Right x) (Right y) = diffDData name x y
diffDTrees' name _ _ = [showName name ++ " have different types"]

diffDFields :: [String] -> DField -> DField -> Maybe String
diffDFields name (DDouble x) (DDouble y)       = diffEq name x y
diffDFields name (DFloat  x) (DFloat  y)       = diffEq name x y
diffDFields name (DInt8    x) (DInt8    y)     = diffEq name x y
diffDFields name (DInt16    x) (DInt16    y)   = diffEq name x y
diffDFields name (DInt32    x) (DInt32    y)   = diffEq name x y
diffDFields name (DInt64    x) (DInt64    y)   = diffEq name x y
diffDFields name (DWord8    x) (DWord8    y)   = diffEq name x y
diffDFields name (DWord16    x) (DWord16    y) = diffEq name x y
diffDFields name (DWord32    x) (DWord32    y) = diffEq name x y
diffDFields name (DWord64    x) (DWord64    y) = diffEq name x y
diffDFields name (DString x) (DString y)       = diffEq name x y
diffDFields name DSorry DSorry                 = Just (showName name ++ ": can't diff this type")
diffDFields name x y
  | sameDFieldType x y = Just $ showName name ++ ": ERROR! unhandled type " ++ show (x, y)
  | otherwise = Just $ showName name ++ ": has different types"

diffEq :: (Eq a, Show a) => [String] -> a -> a -> Maybe String
diffEq name x y
  | x == y = Nothing
  | otherwise = Just (showName name ++ ": " ++ show x ++ " /= " ++ show y)


data MaybeRecords
  = Record [(String, DTree)]
  | NoRecord [DTree]
  | Mixed
  | EmptyCon

toMaybeRecords :: [(Maybe String, DTree)] -> MaybeRecords
toMaybeRecords xs = case partitionEithers (map f xs) of
  ([], []) -> EmptyCon
  ([], r) -> Record r
  (r, []) -> NoRecord r
  _ -> Mixed
  where
    f (Just x, t) = Right (x, t)
    f (Nothing, t) = Left t

diffDData :: [String] -> DData -> DData -> [String]
diffDData name (DData dx (DSum sx@(DSimpleEnum csx kx))) (DData dy (DSum sy@(DSimpleEnum csy ky)))
  | (dx /= dy) || (csx /= csy) = [showName name ++ " have different types"]
  | kx /= ky = case (denumToString sx, denumToString sy) of
      (Right nx, Right ny) -> [showName name ++ ": " ++ nx ++ " /= " ++ ny]
      (nx, ny) -> [showName name ++ ": ERROR converting to enum! " ++ intercalate ", " (lefts [nx, ny])]
  | otherwise = []
diffDData name (DData dx (DConstructor cx xs)) (DData dy (DConstructor cy ys))
  | (dx, cx) /= (dy, cy) = [showName name ++ " has different types " ++ show ((dx, cx), (dy, cy))]
  | otherwise = case (toMaybeRecords xs, toMaybeRecords ys) of
      (Mixed, Mixed) -> [showName name ++ " has mixed types WTF"]
      (EmptyCon, EmptyCon) -> []
      (NoRecord x, NoRecord y)
        | length x == length y ->
            let diffChild k = diffDTrees' (arrayName k:name)
            in concat $ zipWith3 diffChild [0..] x y
        | otherwise -> [showName name ++ " has different types"]
      (Record x, Record y)
        | map fst x /= map fst y -> [showName name ++ " has different types"]
        | otherwise ->
            let diffChild (nx, x') (ny, y')
                  | nx == ny = diffDTrees' (nx:name) x' y'
                  | otherwise = error $ "internal error: record names don't match " ++ show (nx, ny)
            in concat $ zipWith diffChild x y
      _ -> [showName name ++ " has different types"]
diffDData name _ _ = [showName name ++ " has different types"]

--  | otherwise = zipWith diffDData
arrayName :: Int -> String
arrayName k = '[':(show k ++ "]")


lefts :: [Either a b] -> [a]
lefts ((Left x):xs) = x:lefts xs
lefts ((Right _):xs) = lefts xs
lefts [] = []
