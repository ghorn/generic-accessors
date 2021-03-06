{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )

import Data.Int ( Int8, Int16 )
import Data.Word ( Word64 )
import Data.Monoid ( mempty )
import Text.Printf ( printf )
import Test.Framework ( ColorMode(..), RunnerOptions'(..), TestOptions'(..), defaultMainWithOpts )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Accessors
import Accessors.Dynamic

main :: IO ()
main = do
  defaultMainWithOpts
    [ accessorTests
    , dynamicTests
    ]
    opts

opts :: RunnerOptions' Maybe
opts = mempty { ropt_color_mode = Just ColorAlways
              , ropt_threads = Just 1
              , ropt_test_options = Just my_test_opts
              }

my_test_opts :: TestOptions' Maybe
my_test_opts = mempty { topt_timeout = Just (Just 2000000) }


data Xyz a = Xyz { xx :: Int8
                 , yy :: Double
                 , zz :: Float
                 , bb :: (Bool, Bool, Bool)
                 , ww :: a
                 } deriving (Generic, Show)
data One = MkOne { one :: Double
                 } deriving (Generic, Show)
data Foo = MkFoo { aaa :: Int16
                 , bbb :: Xyz Word64
                 , lol :: Bool
                 , notlol :: Bool
                 , yoyo :: Xyz (Xyz Double)
                 , ccc :: One
                 } deriving (Generic, Show)
instance Lookup One
instance Lookup a => Lookup (Xyz a)
instance Lookup Foo

yo :: Xyz (Xyz Double)
yo = Xyz 42 45 2000 (True, True, False) (Xyz 2 3 4 (False, True, False) 5)

foo :: Foo
foo = MkFoo 2 (Xyz 6 7 8 (True, False, False) 9) True False yo (MkOne 17)

yup :: Either (GAField Foo) (GAData Foo)
yup = accessors


accessorTests :: Test
accessorTests =
  testGroup "accessors"
  [ testCase "showTree" treeTest
  , testCase "showFlat" flatTest
  , testCase "showFlat (aligned)" flatTestAligned
  ]

dynamicTests :: Test
dynamicTests =
  testGroup "dynamic"
  [ testCase "toDValue" toDValueTest
  , testCase "updateDValueTest" updateDValueTest
  , testCase "badupdateDValueTest" badUpdateDValueTest
  ]

assertEqualString :: String -> String -> HUnit.Assertion
assertEqualString x y = HUnit.assertBool msg (x == y)
  where
    msg = "------------------ expected: ------------------\n" ++
          x ++
          "\n------------------ but got: -------------------\n" ++
          y ++
          "\n-----------------------------------------------"

assertEqualString' :: Either String String -> Either String String -> HUnit.Assertion
assertEqualString' ex ey = HUnit.assertBool msg (ex == ey)
  where
    x = case ex of
      Left r -> r
      Right r -> r
    y = case ey of
      Left r -> r
      Right r -> r
    msg = "------------------ expected: ------------------\n" ++
          x ++
          "\n------------------ but got: -------------------\n" ++
          y ++
          "\n-----------------------------------------------"

toDValueTest :: HUnit.Assertion
toDValueTest = assertEqualString x y
  where
    x = "Right (DData \"Xyz\" (DConstructor \"Xyz\" [(Just \"xx\",Left (DInt8 1)),(Just \"yy\",Left (DDouble 2.0)),(Just \"zz\",Left (DFloat 3.0)),(Just \"bb\",Right (DData \"(,,)\" (DConstructor \"(,,)\" [(Just \"(x,_,_)\",Left (DBool True)),(Just \"(_,x,_)\",Left (DBool False)),(Just \"(_,_,x)\",Left (DBool True))]))),(Just \"ww\",Right (DData \"One\" (DConstructor \"MkOne\" [(Just \"one\",Left (DDouble 4.0))])))]))"

    y = show (toDData (Xyz 1 2 3 (True, False, True) (MkOne 4)))

updateDValueTest :: HUnit.Assertion
updateDValueTest = assertEqualString' (fmap show x) (fmap show y)
  where
    x0 :: Xyz One
    x0 = Xyz 1 2 3 (True, False, True) (MkOne 4)

    dvalue =
      DData "Xyz" $
      DConstructor "Xyz" $
      [ (Just "xx", Left (DInt8 10))
      , (Just "yy", Left (DDouble 20.0))
      , (Just "zz", Left (DFloat 30.0))
      , ( Just "bb"
        , Right (DData "(,,)"
                 (DConstructor "(,,)"
                  [ (Just "(x,_,_)", Left (DBool False))
                  , (Just "(_,x,_)", Left (DBool True))
                  , (Just "(_,_,x)", Left (DBool True))
                  ]))
        )
      , (Just "ww", Right (DData "One"
                           (DConstructor "MkOne"
                            [ (Just "one", Left (DDouble 40.0))
                            ])))
      ]

    x :: Either String (Xyz One)
    x = Right $ Xyz 10 20 30 (False, True, True) (MkOne 40)

    y :: Either String (Xyz One)
    y = updateLookupable x0 (Right dvalue)

badUpdateDValueTest :: HUnit.Assertion
badUpdateDValueTest = assertEqualString' (fmap show x) (fmap show y)
  where
    x0 :: Xyz One
    x0 = Xyz 1 2 3 (True, False, True) (MkOne 4)

    dvalue =
      DData "Xyz" $
      DConstructor "Xyz" $
      [ (Just "xx", Left (DInt8 10))
      , (Just "yy", Left (DDouble 20.0))
      , (Just "zz", Left (DFloat 30.0))
      , ( Just "b"
        , Right (DData "(,,)"
                 (DConstructor "(,,)"
                  [ (Just "(x,_,_)", Left (DBool False))
                  , (Just "(_,x,_)", Left (DBool True))
                  , (Just "(_,_,x)", Left (DBool True))
                  ]))
        )
      , (Just "ww", Right (DData "One"
                           (DConstructor "MkOne"
                            [ (Just "one", Left (DDouble 40.0))
                            ])))
      ]

    x :: Either String (Xyz One)
    x = Left "accessor selector name Just \"bb\" doesn't match dynamic selector name Just \"b\""

    y :: Either String (Xyz One)
    y = updateLookupable x0 (Right dvalue)

treeTest :: HUnit.Assertion
treeTest = assertEqualString x y
  where
    x = init $ unlines
        [ "MkFoo"
        , "{ aaa = 2"
        , ", bbb = Xyz"
        , "        { xx = 6"
        , "        , yy = 7.00e0"
        , "        , zz = 8.00e0"
        , "        , bb = (,,)"
        , "               { (x,_,_) = True"
        , "               , (_,x,_) = False"
        , "               , (_,_,x) = False"
        , "               }"
        , "        , ww = 9"
        , "        }"
        , ", lol = True"
        , ", notlol = False"
        , ", yoyo = Xyz"
        , "         { xx = 42"
        , "         , yy = 4.50e1"
        , "         , zz = 2.00e3"
        , "         , bb = (,,)"
        , "                { (x,_,_) = True"
        , "                , (_,x,_) = True"
        , "                , (_,_,x) = False"
        , "                }"
        , "         , ww = Xyz"
        , "                { xx = 2"
        , "                , yy = 3.00e0"
        , "                , zz = 4.00e0"
        , "                , bb = (,,)"
        , "                       { (x,_,_) = False"
        , "                       , (_,x,_) = True"
        , "                       , (_,_,x) = False"
        , "                       }"
        , "                , ww = 5.00e0"
        , "                }"
        , "         }"
        , ", ccc = MkOne"
        , "        { one = 1.70e1"
        , "        }"
        , "}"
        ]
    y = showTree yup (printf "%.2e") foo

flatTest :: HUnit.Assertion
flatTest = assertEqualString x y
  where
    x = init $ unlines
        [ "aaa = 2"
        , "bbb.xx = 6"
        , "bbb.yy = 7.00e0"
        , "bbb.zz = 8.00e0"
        , "bbb.bb.(x,_,_) = True"
        , "bbb.bb.(_,x,_) = False"
        , "bbb.bb.(_,_,x) = False"
        , "bbb.ww = 9"
        , "lol = True"
        , "notlol = False"
        , "yoyo.xx = 42"
        , "yoyo.yy = 4.50e1"
        , "yoyo.zz = 2.00e3"
        , "yoyo.bb.(x,_,_) = True"
        , "yoyo.bb.(_,x,_) = True"
        , "yoyo.bb.(_,_,x) = False"
        , "yoyo.ww.xx = 2"
        , "yoyo.ww.yy = 3.00e0"
        , "yoyo.ww.zz = 4.00e0"
        , "yoyo.ww.bb.(x,_,_) = False"
        , "yoyo.ww.bb.(_,x,_) = True"
        , "yoyo.ww.bb.(_,_,x) = False"
        , "yoyo.ww.ww = 5.00e0"
        , "ccc.one = 1.70e1"
        ]
    y = showFlat yup False (printf "%.2e") foo

flatTestAligned :: HUnit.Assertion
flatTestAligned = assertEqualString x y
  where
    x = init $ unlines
        [ "aaa                = 2"
        , "bbb.xx             = 6"
        , "bbb.yy             = 7.00e0"
        , "bbb.zz             = 8.00e0"
        , "bbb.bb.(x,_,_)     = True"
        , "bbb.bb.(_,x,_)     = False"
        , "bbb.bb.(_,_,x)     = False"
        , "bbb.ww             = 9"
        , "lol                = True"
        , "notlol             = False"
        , "yoyo.xx            = 42"
        , "yoyo.yy            = 4.50e1"
        , "yoyo.zz            = 2.00e3"
        , "yoyo.bb.(x,_,_)    = True"
        , "yoyo.bb.(_,x,_)    = True"
        , "yoyo.bb.(_,_,x)    = False"
        , "yoyo.ww.xx         = 2"
        , "yoyo.ww.yy         = 3.00e0"
        , "yoyo.ww.zz         = 4.00e0"
        , "yoyo.ww.bb.(x,_,_) = False"
        , "yoyo.ww.bb.(_,x,_) = True"
        , "yoyo.ww.bb.(_,_,x) = False"
        , "yoyo.ww.ww         = 5.00e0"
        , "ccc.one            = 1.70e1"
        ]
    y = showFlat yup True (printf "%.2e") foo
