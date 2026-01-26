{-# OPTIONS_GHC -Wno-orphans #-}

module Hasql.PostgresqlTypes.Mapping where

import GHC.TypeLits (KnownNat)
import qualified Hasql.Mapping
import qualified Hasql.PostgresqlTypes.Core as Core
import PostgresqlTypes
import PostgresqlTypes.Algebra

instance (KnownNat len) => Hasql.Mapping.IsScalar (Bit len) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Bool where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Box where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (KnownNat len) => Hasql.Mapping.IsScalar (Bpchar len) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Bytea where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Char where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Cidr where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Circle where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Date where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Float4 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Float8 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Hstore where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Inet where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Int2 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Int4 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Int8 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Interval where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Json where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Jsonb where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Line where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Lseg where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Macaddr where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Macaddr8 where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Money where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (IsMultirangeElement element) => Hasql.Mapping.IsScalar (Multirange element) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (KnownNat precision, KnownNat scale) => Hasql.Mapping.IsScalar (Numeric precision scale) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Oid where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Path where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Point where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Polygon where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (IsRangeElement element) => Hasql.Mapping.IsScalar (Range element) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Text where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Time where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Timestamp where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Timestamptz where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Timetz where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance Hasql.Mapping.IsScalar Uuid where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (KnownNat maxLen) => Hasql.Mapping.IsScalar (Varbit maxLen) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder

instance (KnownNat maxLen) => Hasql.Mapping.IsScalar (Varchar maxLen) where
  encoderOf = Core.encoder
  decoderOf = Core.decoder
