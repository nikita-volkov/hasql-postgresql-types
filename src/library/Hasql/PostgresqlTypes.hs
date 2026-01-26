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
-- > import qualified Hasql.Mapping as Mapping
-- >
-- > myStatement :: Statement.Statement Pt.Timestamptz [Pt.Timestamptz]
-- > myStatement = Statement.preparable sql enc dec
-- >   where
-- >     sql = "SELECT $1::timestamptz"
-- >     enc = Encoders.param (Encoders.nonNullable Mapping.encoderOf)
-- >     dec = Decoders.rowList (Decoders.column (Decoders.nonNullable Mapping.decoderOf))
--
-- The 'encoderOf' and 'decoderOf' functions work with any type having an 'IsScalar' instance,
-- automatically handling binary encoding/decoding and OID resolution.
module Hasql.PostgresqlTypes () where

import Hasql.PostgresqlTypes.Mapping ()
