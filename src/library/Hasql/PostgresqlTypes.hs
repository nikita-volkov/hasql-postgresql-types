{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides a bridge between the types defined in the ["postgresql-types"](https://hackage.haskell.org/package/postgresql-types) and the ["hasql"](https://hackage.haskell.org/package/hasql) library,
-- offering automatic encoder and decoder generation for types that implement the 'IsScalar' constraint.
--
-- == Usage Example
--
-- > import Hasql.PostgresqlTypes ()
-- > import qualified PostgresqlTypes as Pt
-- > import qualified Hasql.Statement as Statement
-- > import qualified Hasql.Encoders as Encoders
-- > import qualified Hasql.Decoders as Decoders
-- > import qualified Hasql.Mapping.IsScalar as IsScalar
-- >
-- > myStatement :: Statement.Statement Pt.Timestamptz [Pt.Timestamptz]
-- > myStatement = Statement.preparable sql enc dec
-- >   where
-- >     sql = "SELECT $1::timestamptz"
-- >     enc = Encoders.param (Encoders.nonNullable IsScalar.encoder)
-- >     dec = Decoders.rowList (Decoders.column (Decoders.nonNullable IsScalar.decoder))
--
-- The 'encoder' and 'decoder' functions work with any type having an 'IsScalar' instance,
-- automatically handling binary encoding/decoding and OID resolution.
module Hasql.PostgresqlTypes () where

import GHC.TypeLits (KnownNat)
import qualified Hasql.Mapping.IsScalar as IsScalar
import qualified Hasql.PostgresqlTypes.Core as Core
import PostgresqlTypes
import PostgresqlTypes.Algebra

instance (KnownNat len) => IsScalar.IsScalar (Bit len) where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Bool where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Box where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat len) => IsScalar.IsScalar (Bpchar len) where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Bytea where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Char where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Cidr where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Circle where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Date where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Float4 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Float8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Hstore where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Inet where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Int2 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Int4 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Int8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Interval where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Json where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Jsonb where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Line where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Lseg where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Macaddr where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Macaddr8 where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Money where
  encoder = Core.encoder
  decoder = Core.decoder

instance (IsMultirangeElement element) => IsScalar.IsScalar (Multirange element) where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat precision, KnownNat scale) => IsScalar.IsScalar (Numeric precision scale) where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Oid where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Path where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Point where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Polygon where
  encoder = Core.encoder
  decoder = Core.decoder

instance (IsRangeElement element) => IsScalar.IsScalar (Range element) where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Text where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Time where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Timestamp where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Timestamptz where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Timetz where
  encoder = Core.encoder
  decoder = Core.decoder

instance IsScalar.IsScalar Uuid where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat maxLen) => IsScalar.IsScalar (Varbit maxLen) where
  encoder = Core.encoder
  decoder = Core.decoder

instance (KnownNat maxLen) => IsScalar.IsScalar (Varchar maxLen) where
  encoder = Core.encoder
  decoder = Core.decoder
