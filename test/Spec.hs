{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Proxy
import qualified Data.Swagger as S
import Composite.Record
import Composite.Swagger ()
import qualified Control.Lens as L
import qualified Data.HashMap.Strict.InsOrd as IOHM

main :: IO ()
main = hspec spec

type TestRec = Record
  '[ "foo" :-> Int
   , "bar" :-> Bool
   , "optional" :-> Maybe Int ]

spec :: Spec
spec = do
  describe "ToSchemaRecord instance" $
    it "should generate the schema" $ do
      let schema = S.toSchema (Proxy @TestRec)
      IOHM.keys (L.view S.properties schema)
        `shouldBe` ["foo", "bar", "optional"]
      L.view S.required schema
        `shouldBe` ["foo", "bar"]
      L.view S.type_ schema
        `shouldBe` S.SwaggerObject
