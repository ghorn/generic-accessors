{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Accessors.Dynamic
       ( DData(..), DConstructor(..), DSimpleEnum(..), DField(..)
       , toDData, updateLookupable
       ) where

import GHC.Generics

import Data.Data ( Data )
import Data.List ( intercalate )
import Data.Typeable ( Typeable )
import Control.Lens

import Accessors

data DData = DData String DConstructor
           deriving (Generic, Show, Eq, Ord, Data, Typeable)

data DSimpleEnum = DSimpleEnum [String] Int
                 deriving (Generic, Show, Eq, Ord, Data, Typeable)

data DConstructor = DConstructor String [(Maybe String, Either DField DData)]
                  | DSum  DSimpleEnum
                  deriving (Generic, Show, Eq, Ord, Data, Typeable)

-- | a dynamic field
data DField =
  DBool Bool
  | DInt Int
  | DDouble Double
  | DFloat Float
  | DString String
  | DSorry
  deriving (Generic, Show, Eq, Ord, Data, Typeable)


describeDField :: DField -> String
describeDField (DBool _) = "Bool"
describeDField (DInt _) = "Int"
describeDField (DDouble _) = "Double"
describeDField (DFloat _) = "Float"
describeDField (DString _) = "String"
describeDField DSorry = "Sorry"

-- | convert to a dynamic value
toDData :: forall a . Lookup a => a -> Either DField DData
toDData x = toDData' accessors
  where
    toDData' :: Either (GAField a) (GAData a) -> Either DField DData
    toDData' (Right (GAData dname constructor)) =
      Right $ DData dname (toDConstructor constructor)
    toDData' (Left field) = Left (toDField field)

    toDConstructor :: GAConstructor a -> DConstructor
    toDConstructor (GASum (GASimpleEnum names f)) =
      DSum (DSimpleEnum names (x ^. f))
      
    toDConstructor (GAConstructor cname fields) =
      DConstructor cname $ map (\(n, f) -> (n, toDData' f)) fields

    toDField :: GAField a -> DField
    toDField (FieldInt f) = DInt (x ^. f)
    toDField (FieldDouble f) = DDouble (x ^. f)
    toDField (FieldFloat f) = DFloat (x ^. f)
    toDField (FieldString f) = DString (x ^. f)
    toDField FieldSorry = DSorry


-- | Update something using a dynamic representation
updateLookupable :: Lookup a => a -> Either DField DData -> Either String a
updateLookupable x0 dtree = updateData x0 accessors dtree

updateData :: forall a
                     . a
                     -> Either (GAField a) (GAData a)
                     -> Either DField DData
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
updateConstructor x (GASum (GASimpleEnum anames intLens)) (DSum (DSimpleEnum dnames k))
  | anames /= dnames =
      Left $
      "accessor sum options " ++ showList' anames ++
      " doesn't match dynamic sum options " ++ showList' dnames
  | otherwise = Right $ (intLens .~ k) x
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
           -> [(Maybe String, Either DField DData)]
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
updateConstructor _ (GASum (GASimpleEnum anames _)) (DConstructor dconName _) =
  Left $ "got GASum ([" ++ showList' anames ++ "]) but DConstructor (" ++ dconName ++ ")"


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
