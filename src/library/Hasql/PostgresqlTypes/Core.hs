-- |
-- Internal implementation of the hasql encoder and decoder for types
-- that implement the 'IsScalar' constraint from
-- ["postgresql-types-algebra"](https://hackage.haskell.org/package/postgresql-types-algebra).
-- These are used by the 'IsScalar.IsScalar' instances defined in
-- "Hasql.PostgresqlTypes".
--
-- The 'encoder' and 'decoder' functions work with any type having an 'IsScalar' instance,
-- automatically handling binary encoding/decoding and OID resolution.
module Hasql.PostgresqlTypes.Core
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
