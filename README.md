# hasql-postgresql-types

[![Hackage](https://img.shields.io/hackage/v/hasql-postgresql-types.svg)](https://hackage.haskell.org/package/hasql-postgresql-types)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/hasql-postgresql-types/)

Integration of ["hasql"](https://github.com/nikita-volkov/hasql) with ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) via the `IsScalar` typeclass from ["postgresql-types-algebra"](https://github.com/nikita-volkov/postgresql-types-algebra).
Provides automatic encoder and decoder generation for precise PostgreSQL scalar types.

## Usage

```haskell
import Hasql.PostgresqlTypes (encoder, decoder)
import qualified PostgresqlTypes as Pt
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

myStatement :: Statement.Statement Pt.Timestamptz [Pt.Timestamptz]
myStatement = Statement.preparable sql enc dec
  where
    sql = "SELECT $1::timestamptz"
    enc = Encoders.param (Encoders.nonNullable encoder)
    dec = Decoders.rowList (Decoders.column (Decoders.nonNullable decoder))
```
