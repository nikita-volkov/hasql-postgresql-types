-- |
-- This module provides a bridge between PostgreSQL's standard types and the Hasql library,
-- offering automatic encoder and decoder generation for types that implement the 'IsScalar' constraint.
module Hasql.PostgresqlTypes
  ( IsScalar,
    encoder,
    decoder,
  )
where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.PostgresqlTypes.Prelude
import PostgresqlTypes.Algebra (IsScalar (..))
import qualified PtrPeeker
import qualified PtrPoker.Write as Write

-- | Hasql value encoder for a PostgreSQL standard type.
encoder :: forall a. (IsScalar a) => Encoders.Value a
encoder =
  Encoders.custom
    Nothing
    (untag (typeName @a))
    ((,) <$> untag (baseOid @a) <*> untag (arrayOid @a))
    []
    (\_ value -> Write.writeToByteString (binaryEncoder value))
    (to . textualEncoder)

-- | Hasql value decoder for a PostgreSQL standard type.
decoder :: forall a. (IsScalar a) => Decoders.Value a
decoder =
  Decoders.custom
    Nothing
    (untag (typeName @a))
    ((,) <$> untag (baseOid @a) <*> untag (arrayOid @a))
    []
    ( \_ bytes ->
        case PtrPeeker.runVariableOnByteString binaryDecoder bytes of
          Left bytesUnconsumed -> Left (onto ("Binary decoder did not consume all input bytes, unconsumed bytes: " ++ show bytesUnconsumed))
          Right (Left err) -> Left (onto (show err))
          Right (Right value) -> Right value
    )
