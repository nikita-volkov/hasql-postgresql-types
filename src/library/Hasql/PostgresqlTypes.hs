-- |
-- This module provides a bridge between PostgreSQL's standard types and the Hasql library,
-- offering automatic encoder and decoder generation for types that implement the 'IsScalar' constraint.
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
