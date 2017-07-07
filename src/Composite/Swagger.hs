-- | 'ToSchema' instances for Composite records.
{-# LANGUAGE OverloadedLists #-}
module Composite.Swagger where

import Data.Proxy
import GHC.TypeLits
import Composite
import Control.Lens ((&))
import qualified Data.Singletons.Bool as SB
import qualified Data.Text as T
import qualified Control.Lens as L
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S

-- Hmmm, this orphan instance may not be the best idea, since it covers all
-- Vinyl Identity records. It's more in the spirit of Composite to expose only
-- 'declareNamedSchemaRecord'.
instance ToSchemaRecord rs => S.ToSchema (Record rs) where
  declareNamedSchema = declareNamedSchemaRecord

-- | Default implementation of 'S.declareNamedSchema' for Composite records.
declareNamedSchemaRecord :: forall rs proxy.
     ToSchemaRecord rs
  => proxy (Record rs)
  -> S.Declare (S.Definitions S.Schema) S.NamedSchema
declareNamedSchemaRecord _ = unnamedSchema <$> declareSchemaRecord (Proxy @rs)
  where
    unnamedSchema = S.NamedSchema Nothing

class ToSchemaRecord (rs :: [*]) where
  declareSchemaRecord :: proxy rs -> S.Declare (S.Definitions S.Schema) S.Schema

instance ToSchemaRecord '[] where
  declareSchemaRecord _ = pure mempty

instance (KnownSymbol label, S.ToSchema a, ToSchemaRecord rs, SB.SBoolI (IsRequired a))
      => ToSchemaRecord ((label :-> a) : rs) where

  declareSchemaRecord _ = do
    valueSchema <- S.declareSchemaRef (Proxy @a)
    let
      fieldName = T.pack $ symbolVal (Proxy @label)
      isRequired = sboolToBool (SB.sbool @(IsRequired a))
      fieldSchema = mempty
        & L.set S.properties [ (fieldName, valueSchema) ]
        & L.set S.required (if isRequired then [ fieldName ] else [])

    restSchema <- declareSchemaRecord (Proxy @rs)
    pure $ fieldSchema `mappend` restSchema

type family IsRequired (ty :: *) :: Bool where
  IsRequired (Maybe a) = False
  IsRequired a = True

sboolToBool :: SB.SBool b -> Bool
sboolToBool = \case
  SB.STrue -> True
  SB.SFalse -> False
