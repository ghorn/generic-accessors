{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import GHC.Generics ( Generic )

import Data.Monoid ( mempty )
import Text.Printf ( printf )
import Test.Framework ( ColorMode(..), RunnerOptions'(..), TestOptions'(..), defaultMainWithOpts )
import qualified Test.HUnit.Base as HUnit
import Test.Framework ( Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )

import Accessors

main :: IO ()
main = do
  defaultMainWithOpts
    [ accessorTests
    ]
    opts

opts :: RunnerOptions' Maybe
opts = mempty { ropt_color_mode = Just ColorAlways
              , ropt_threads = Just 1
              , ropt_test_options = Just my_test_opts
              }

my_test_opts :: TestOptions' Maybe
my_test_opts = mempty { topt_timeout = Just (Just 2000000) }


data Xyz a = Xyz { xx :: Int
                 , yy :: Double
                 , zz :: Float
                 , bb :: (Bool, Bool, Bool)
                 , ww :: a
                 } deriving (Generic, Show)
data One = MkOne { one :: Double
                 } deriving (Generic, Show)
data Foo = MkFoo { aaa :: Int
                 , bbb :: Xyz Int
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

assertEqualString :: String -> String -> HUnit.Assertion
assertEqualString x y = HUnit.assertBool msg (x == y)
  where
    msg = "------------------ expected: ------------------\n" ++
          x ++
          "\n------------------ but got: -------------------\n" ++
          y ++
          "\n-----------------------------------------------"

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
