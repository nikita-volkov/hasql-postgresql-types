# v0.2.1.0

## Non-breaking

- Upgraded to `postgresql-types-algebra` v0.2 and `postgresql-types` v0.1.5.
- Added `IsScalar` instances for `Citext` and `Geometry`.

# v0.2

## Breaking

- Removed the `encoder` and `decoder` functions from `Hasql.PostgresqlTypes`. Codec access is now done via the `IsScalar` typeclass from the new [`hasql-mapping`](https://hackage.haskell.org/package/hasql-mapping) dependency. Import `Hasql.PostgresqlTypes ()` to bring the instances into scope, then use `Hasql.Mapping.IsScalar.encoder` and `Hasql.Mapping.IsScalar.decoder` (or the re-exports from `Hasql.Mapping`).
- Bumped `hasql` lower bound from `1.10.1` to `1.10.3`.

## Non-breaking

- Added `IsScalar` instances for all PostgreSQL types defined in [`postgresql-types`](https://hackage.haskell.org/package/postgresql-types), including `Tsvector`.
- Added new dependencies: [`hasql-mapping`](https://hackage.haskell.org/package/hasql-mapping) and [`postgresql-types`](https://hackage.haskell.org/package/postgresql-types).

# v0.1.0.1

Initial release.
