{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Accessors.Dynamic
       ( DTree, DData(..), DConstructor(..), DSimpleEnum(..), DField(..)
       , toDData, updateLookupable, describeDField, sameDFieldType
         -- * some utility functions for working with DSimpleEnums
       , denumToString, denumToStringOrMsg, denumSetString, denumSetIndex
       ) where

import GHC.Generics

import Data.Binary ( Binary )
import Data.Serialize ( Serialize )
import Data.Data ( Data )
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
  | DInt Int
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
sameDFieldType (DFloat _) (DFloat _) = True
sameDFieldType (DInt _) (DInt _) = True
sameDFieldType (DString _) (DString _) = True
sameDFieldType DSorry DSorry = True
sameDFieldType (DDouble _) _ = False
sameDFieldType (DFloat _) _ = False
sameDFieldType (DInt _) _ = False
sameDFieldType (DString _) _ = False
sameDFieldType DSorry _ = False

describeDField :: DField -> String
describeDField (DInt _) = "Int"
describeDField (DDouble _) = "Double"
describeDField (DFloat _) = "Float"
describeDField (DString _) = "String"
describeDField DSorry = "Sorry"

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
    toDField (FieldInt f) = DInt (x ^. f)
    toDField (FieldDouble f) = DDouble (x ^. f)
    toDField (FieldFloat f) = DFloat (x ^. f)
    toDField (FieldString f) = DString (x ^. f)
    toDField FieldSorry = DSorry


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
updateField x0 (FieldFloat f) (DFloat x) = Right $ (f .~ x) x0
updateField x0 (FieldInt f) (DInt x) = Right $ (f .~ x) x0
updateField x0 (FieldString f) (DString x) = Right $ (f .~ x) x0
updateField x0 FieldSorry _ = Right x0
updateField _ f@(FieldDouble _) d = Left (fieldMismatch f d)
updateField _ f@(FieldFloat _) d = Left (fieldMismatch f d)
updateField _ f@(FieldInt _) d = Left (fieldMismatch f d)
updateField _ f@(FieldString _) d = Left (fieldMismatch f d)

fieldMismatch :: GAField a -> DField -> String
fieldMismatch f d =
  "accessor GAField " ++ describeGAField f ++
  " got incompatible dynamic DField " ++ describeDField d
