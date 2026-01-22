-- |
-- This module provides a bridge between the types defined in the ["postgresql-types"](https://hackage.haskell.org/package/postgresql-types) and the ["hasql"](https://hackage.haskell.org/package/hasql) library,
-- offering automatic encoder and decoder generation for types that implement the 'IsScalar' constraint.
--
-- == Usage Example
--
-- > import Hasql.PostgresqlTypes (encoder, decoder)
-- > import qualified PostgresqlTypes as Pt
-- > import qualified Hasql.Statement as Statement
-- > import qualified Hasql.Encoders as Encoders
-- > import qualified Hasql.Decoders as Decoders
-- >
-- > myStatement :: Statement.Statement Pt.Timestamptz [Pt.Timestamptz]
-- > myStatement = Statement.preparable sql enc dec
-- >   where
-- >     sql = "SELECT $1::timestamptz"
-- >     enc = Encoders.param (Encoders.nonNullable encoder)
-- >     dec = Decoders.rowList (Decoders.column (Decoders.nonNullable decoder))
--
-- The 'encoder' and 'decoder' functions work with any type having an 'IsScalar' instance,
-- automatically handling binary encoding/decoding and OID resolution.
module Hasql.PostgresqlTypes
  ( encoder,
    decoder,
  )
where

import Data.String (fromString)
import Data.Tagged (untag)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import PostgresqlTypes.Algebra (IsScalar (..))
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder
import Prelude

-- | Hasql value encoder for a PostgreSQL standard type.
--
-- Generates a Hasql value encoder for any type implementing 'IsScalar'.
-- The encoder handles type resolution, and binary encoding automatically.
encoder :: forall a. (IsScalar a) => Encoders.Value a
encoder =
  Encoders.custom
    Nothing
    (untag (typeName @a))
    ((,) <$> untag (baseOid @a) <*> untag (arrayOid @a))
    []
    (\_ value -> Write.toByteString (binaryEncoder value))
    (TextBuilder.toText . textualEncoder)

-- | Hasql value decoder for a PostgreSQL standard type.
--
-- Generates a Hasql value decoder for any type implementing 'IsScalar'.
-- The decoder handles type resolution, and binary decoding with proper error handling.
decoder :: forall a. (IsScalar a) => Decoders.Value a
decoder =
  Decoders.custom
    Nothing
    (untag (typeName @a))
    ((,) <$> untag (baseOid @a) <*> untag (arrayOid @a))
    []
    ( \_ bytes ->
        case PtrPeeker.runVariableOnByteString binaryDecoder bytes of
          Left bytesUnconsumed -> Left ("Binary decoder did not consume all input bytes, unconsumed bytes: " <> fromString (show bytesUnconsumed))
          Right (Left err) -> Left (fromString (show err))
          Right (Right value) -> Right value
    )
