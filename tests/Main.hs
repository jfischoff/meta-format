{-# LANGUAGE CPP, GADTs, TemplateHaskell, QuasiQuotes, StandaloneDeriving,
    OverloadedStrings #-}
{-# OPTIONS_GHC -pgmPcpphs  -optP--cpp #-}
module Main where
import Data.Archive.Meta
import Test.Framework (defaultMain, testGroup, Test(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertBool, (@?=))
import Test.QuickCheck
import System.Console.Terminfo.Color
import Text.PrettyPrint.Free
import System.Console.Terminfo.Base
import Data.Generic.Diff
import System.Console.Terminfo.PrettyPrint
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Types as Data.Aeson.Types
import Data.Aeson.QQ
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.HashMap.Strict as H
import Data.Text
import Data.Attoparsec.Number 
import qualified Data.Vector as Data.Vector
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Applicative

deriving instance Eq Tar.Entry

--stolen from checkers
(>*<) :: Gen a -> Gen b -> Gen (a,b)
x >*< y = liftM2 (,) x y

#define TEST_CASE(x) testCase #x x
#define TEST_PROP(x) testProperty #x x

main = defaultMain [
            testGroup "tests" [
                TEST_CASE(test_getValue                           ),
                TEST_CASE(test_objectOrSpecial_object             ),
                TEST_CASE(test_objectOrSpecial_InternalReference  ),
                TEST_CASE(test_objectOrSpecial_ExternalReference  ),
                TEST_CASE(test_valueToJSON_object                 ),
                TEST_CASE(test_valueToJSON_array                  ),
                TEST_CASE(test_valueToJSON_string                 ),
                TEST_CASE(test_valueToJSON_number                 ),
                TEST_CASE(test_valueToJSON_bool                   ),
                TEST_CASE(test_valueToJSON_null                   ),
                TEST_CASE(test_valueToJSON_InternalReference      ),
                TEST_CASE(test_valueToJSON_ExternalReference      ),
                TEST_CASE(test_valueFromJSON_object               ),
                TEST_CASE(test_valueFromJSON_array                ),
                TEST_CASE(test_valueFromJSON_string               ),
                TEST_CASE(test_valueFromJSON_number               ),
                TEST_CASE(test_valueFromJSON_bool                 ),
                TEST_CASE(test_valueFromJSON_null                 ),
                TEST_CASE(test_valueFromJSON_InternalReference    ),
                TEST_CASE(test_valueFromJSON_ExternalReference    ),
                TEST_CASE(test_contextToFiles                     )
                --TEST_CASE(test_contextToTar                       )
            ],
            testGroup "props" [
                TEST_PROP(prop_valueToJsonFromJson )
            ]
        ]

pprEdits :: EditScriptL f txs tys -> TermDoc
pprEdits x = case x of 
    Cpy c d   -> (text $ string c) <+> pprEdits d
    CpyTree d -> text " ... "      <+> pprEdits d
    Del c d   -> (with (Foreground Red)   . text $ "- " ++ string c) <+> pprEdits d
    Ins c d   -> (with (Foreground Green) . text $ "+ " ++ string c) <+> pprEdits d
    End       -> line
   
assertDiff :: (Family f, Type f a, Eq a) 
           => (a -> a -> EditScript f a a) -> a -> a -> Assertion
assertDiff d x y = assertBool message (x == y) where
    message = ($"") . displayS . renderCompact . pprEdits . compress $ d x y
        
runParserObject :: Object -> (Object -> A.Parser a) -> a
runParserObject initial p = case A.parse p initial of
                                A.Success x -> x
                                A.Error msg -> error msg
                                
instance Arbitrary Text where
    arbitrary = pack <$> arbitrary
    
instance Arbitrary Number where
    arbitrary = oneof [
            D <$> arbitrary,
            I <$> arbitrary
        ]    
        
instance Arbitrary A.Value where
    arbitrary = sized arb where
        
        arb i = case i of
            0 -> arbBase
            n -> do
                listSize <- choose(0, n)
                oneof [
                    arbBase, oneof [
                        fmap (A.Object . H.fromList)          $
                            vectorOf listSize (arbitrary >*< arb (n `div` listSize)),
                        fmap (A.Array . Data.Vector.fromList) $
                            vectorOf listSize (arb (n `div` listSize))
                        ]
                    ] 
                
        arbBase = oneof [
                    A.Bool   <$> arbitrary,
                    A.Number <$> arbitrary,
                    A.String <$> arbitrary,
                    return A.Null
            ]
        
instance Arbitrary Value where
    arbitrary = sized arb where
        
        arb i = case i of
            0 -> arbBase
            n -> do
                listSize <- choose(0, n)
                oneof [
                    arbBase, oneof [
                        fmap (Object . H.fromList)          $
                            vectorOf listSize (arbitrary >*< arb (n `div` listSize)),
                        fmap (Array . Data.Vector.fromList) $
                            vectorOf listSize (arb (n `div` listSize))
                        ]
                    ] 
                
        arbBase = oneof [
                    Bool   <$> arbitrary,
                    Number <$> arbitrary,
                    String <$> arbitrary,
                    InternalReference <$> arbitrary,
                    liftM2 ExternalReference arbitrary arbitrary, 
                    return Null
            ]
        
test_getValue = actual @?= expected where
    actual        = runParserObject initialObject (flip getValue key)
    expected      = "yo"
    initialObject = H.fromList [("key", String "yo")]
    key           = "key"

test_objectOrSpecial_object = actual @?= expected where
    actual   = runParserObject initial objectOrSpecial
    expected = Object initial
    initial  = H.fromList [("key", String "yo")]
                          
test_objectOrSpecial_InternalReference = actual @?= expected where
    actual   = runParserObject initial objectOrSpecial
    expected = InternalReference "ref"
    initial  = H.fromList [("__type__", String "InternalReference"), 
                           ("ref"     , String "ref")] 
    
test_objectOrSpecial_ExternalReference = actual @?= expected where
    actual   = runParserObject initial objectOrSpecial
    expected = ExternalReference "png" "ref"
    initial  = H.fromList [("__type__", String "ExternalReference"), 
                           ("ref"     , String "ref"),
                           ("referencedType"     , String "png")]
                 
test_valueToJSON_object = actual @?= expected where
    expected = [aesonQQ|{
           "bool"   : true    ,
           "number" : 1.0     ,
           "string" : "string",
           "null"   : null     
        }|]
    actual   = A.toJSON initial
    initial  = Object $ H.fromList [
            ("bool"  , Bool True),
            ("number", Number $ Data.Attoparsec.Number.D 1.0),
            ("string", String "string"),
            ("null"  , Null)
        ]
                 
test_valueToJSON_array = actual @?= expected where
    expected = [aesonQQ| [0, 1, 2, 3] |]
    actual   = A.toJSON initial
    initial  = [0, 1, 2, 3 :: Int]              

test_valueToJSON_string = actual @?= expected where
    expected = [aesonQQ| "hey" |]
    actual   = A.toJSON initial
    initial  = String "hey"
    
test_valueToJSON_number = actual @?= expected where
    expected = [aesonQQ| 1.0 |]
    actual   = A.toJSON initial
    initial  = 1.0 :: Double

test_valueToJSON_bool = actual @?= expected where
    expected = [aesonQQ| false |]
    actual   = A.toJSON initial
    initial  = False

test_valueToJSON_null = actual @?= expected where
    expected = [aesonQQ| null |]
    actual   = A.toJSON initial
    initial  = Null               

test_valueToJSON_InternalReference = actual @?= expected where
   expected = [aesonQQ|{
        "__type__" : "InternalReference",
        "ref"      : "reference"
   }|]
   actual   = A.toJSON initial
   initial  = InternalReference "reference"
    
test_valueToJSON_ExternalReference = actual @?= expected where
   expected = [aesonQQ|{
        "__type__"       : "ExternalReference",
        "ref"            : "test.png"         ,
        "referencedType" : "png"
    }|]
   actual   = A.toJSON initial
   initial  = ExternalReference "png" "test.png"
   
test_valueFromJSON_object = actual @?= A.Success expected where
    expected = Object $ H.fromList [("innerObject",
        Object $ H.fromList [
            ("bool"  , Bool False),
            ("number", Number $ Data.Attoparsec.Number.I 1),
            ("string", String "yo"),
            ("null"  , Null)
        ])]
    actual   = A.fromJSON initial
    initial  = [aesonQQ|{
        "innerObject" : {
            "bool"   : false,
            "number" : 1    ,
            "string" : "yo" ,
            "null"   : null 
        }
    }|]           
      
test_valueFromJSON_array = actual @?= A.Success expected where
    expected = Array $ Data.Vector.fromList [String "0", String "1", String "2"] 
    actual   = A.fromJSON initial
    initial  = [aesonQQ|["0", "1", "2"]|]            
    
test_valueFromJSON_string = actual @?= A.Success expected where
    expected = String "∆∂ƒ©˙∆"
    actual   = A.fromJSON initial
    initial  = [aesonQQ|"∆∂ƒ©˙∆"|]           
    
test_valueFromJSON_number = actual @?= A.Success expected where
    expected = Number $ Data.Attoparsec.Number.D (-1.99)
    actual   = A.fromJSON initial
    initial  = [aesonQQ|-1.99|]           

test_valueFromJSON_bool = actual @?= A.Success expected where
     expected = Bool False
     actual   = A.fromJSON initial
     initial  = [aesonQQ|false|]            

test_valueFromJSON_null = actual @?= A.Success expected where
    expected = Null
    actual   = A.fromJSON initial
    initial  = [aesonQQ|null|]            

test_valueFromJSON_InternalReference = actual @?= A.Success expected where
    expected = InternalReference "/the/path"
    actual   = A.fromJSON initial
    initial  = [aesonQQ|{
            "__type__" : "InternalReference",
            "ref"      : "/the/path"
        }|]

test_valueFromJSON_ExternalReference = actual @?= A.Success expected where
    expected = ExternalReference "jpg" "/the/path"
    actual   = A.fromJSON initial
    initial  = [aesonQQ|{
        "__type__"       : "ExternalReference",
        "ref"            : "/the/path"        ,
        "referencedType" : "jpg" 
    }|]

test_contextToFiles = actual @?= expected where
    expected = [File "manifest" "null", File "test.png" BL.empty]
    actual   = toFiles initial
    initial  = Context Null [File "test.png" BL.empty]                 

{-
Not sure how to test this yet
test_contextToTar = do
    let expected = undefined
        actual   = toTar initial
        initial  = undefined :: Context   
    assertBool "test_contextToTar" (actual == Right expected)   
-}

prop_valueToJsonFromJson :: Value -> Bool
prop_valueToJsonFromJson x = (A.fromJSON $ A.toJSON x) == A.Success x



   