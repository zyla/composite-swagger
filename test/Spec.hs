{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Hspec
import Data.Proxy
import qualified Data.Swagger as S
import Composite.Record
import Composite.Swagger ()
import qualified Control.Lens as L
import qualified Data.Aeson as A
import Data.Aeson.QQ (aesonQQ)

main :: IO ()
main = hspec spec

type TestRec = Record
  '[ "foo" :-> Integer
   , "bar" :-> Bool
   , "optional" :-> Maybe Integer ]

spec :: Spec
spec = do
  describe "ToSchemaRecord instance" $
    it "should generate the schema" $ do
      A.toJSON (S.toSchema (Proxy @TestRec))
        `shouldBe` [aesonQQ|
           {
             "type": "object",
             "properties": {
                "foo": { "type": "integer" },
                "bar": { "type": "boolean" },
                "optional": { "type": "integer" }
              },
              "required": ["foo", "bar"]
           } |]
