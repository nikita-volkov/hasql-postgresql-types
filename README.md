# hasql-postgresql-types

[![Hackage](https://img.shields.io/hackage/v/hasql-postgresql-types.svg)](https://hackage.haskell.org/package/hasql-postgresql-types)
[![Continuous Haddock](https://img.shields.io/badge/haddock-master-blue)](https://nikita-volkov.github.io/hasql-postgresql-types/)

Integration of ["hasql"](https://github.com/nikita-volkov/hasql) with ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) via the `IsScalar` typeclass from ["postgresql-types-algebra"](https://github.com/nikita-volkov/postgresql-types-algebra).
Provides automatic encoder and decoder generation for precise PostgreSQL scalar types.

## Motivation

The standard "hasql" codecs use common Haskell types like `Text`, `DiffTime`, `UTCTime`, etc. However these types do not always map precisely to PostgreSQL types. E.g., the PostgreSQL `interval` type carries information about months, years and microseconds, while the Haskell `DiffTime` type only represents a time difference in picoseconds. Such mismatches can lead to data loss or unexpected behavior. The ["postgresql-types"](https://github.com/nikita-volkov/postgresql-types) library addresses such issues by providing precise Haskell representations for PostgreSQL types. This package integrates it with "hasql". It also provides a class-based polymorphic interface for defining "hasql" `Value` codecs.

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
