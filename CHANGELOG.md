# Changelog

## 0.2

*Breaking changes*

- Removed the `encoder` and `decoder` functions from `Hasql.PostgresqlTypes`. Codec access is now done via the `IsScalar` typeclass from the new [`hasql-mapping`](https://hackage.haskell.org/package/hasql-mapping) dependency. Import `Hasql.PostgresqlTypes ()` to bring the instances into scope, then use `Hasql.Mapping.IsScalar.encoder` and `Hasql.Mapping.IsScalar.decoder` (or the re-exports from `Hasql.Mapping`).
- Bumped `hasql` lower bound from `1.10.1` to `1.10.3`.

*New features*

- Added `IsScalar` instances for all PostgreSQL types defined in [`postgresql-types`](https://hackage.haskell.org/package/postgresql-types), including `Tsvector`.
- Added new dependencies: [`hasql-mapping`](https://hackage.haskell.org/package/hasql-mapping) and [`postgresql-types`](https://hackage.haskell.org/package/postgresql-types).

## 0.1.0.1

Initial release.
