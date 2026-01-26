{-# OPTIONS_GHC -Wno-orphans #-}

module Hasql.PostgresqlTypes.Mapping where

import GHC.TypeLits (KnownNat)
import qualified Hasql.Mapping
import qualified Hasql.PostgresqlTypes.Core as Core
import PostgresqlTypes
import PostgresqlTypes.Algebra

instance (KnownNat len) => Hasql.Mapping.IsScalar (Bit len) where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Bool where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Box where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat len) => Hasql.Mapping.IsScalar (Bpchar len) where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Bytea where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Char where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Cidr where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Circle where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Date where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Float4 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Float8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Hstore where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Inet where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Int2 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Int4 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Int8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Interval where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Json where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Jsonb where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Line where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Lseg where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Macaddr where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Macaddr8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Money where
  encoder = Core.encoder
  decoder = Core.decoder

instance (IsMultirangeElement element) => Hasql.Mapping.IsScalar (Multirange element) where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat precision, KnownNat scale) => Hasql.Mapping.IsScalar (Numeric precision scale) where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Oid where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Path where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Point where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Polygon where
  encoder = Core.encoder
  decoder = Core.decoder

instance (IsRangeElement element) => Hasql.Mapping.IsScalar (Range element) where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Text where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Time where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Timestamp where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Timestamptz where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Timetz where
  encoder = Core.encoder
  decoder = Core.decoder

instance Hasql.Mapping.IsScalar Uuid where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat maxLen) => Hasql.Mapping.IsScalar (Varbit maxLen) where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat maxLen) => Hasql.Mapping.IsScalar (Varchar maxLen) where
  encoder = Core.encoder
  decoder = Core.decoder
