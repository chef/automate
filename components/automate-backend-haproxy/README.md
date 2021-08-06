# automate-backend-haproxy

This pkg is a wrapper of `core/haproxy`. It serves as a Proxy for the backend PostgreSQL.

## Maintainers

The Habitat Maintainers humans@habitat.sh

## Type of Package

This is a service package.

## Build

In order to build the pkg, you must enter the studio at the root: `a2-ha-backend`

## Bindings

This pkg expects to bind to PostgreSQL as `database` in order to use `port` and `superuser_name`.
It also expects to bind to Pgleaderchk as `pgleaderchk` in order to use `port`.

## Usage

example:
```bash
hab svc load chef/automate-backend-haproxy --bind database:automate-backend-postgresql.default --bind pgleaderchk:automate-backend-pgleaderchk.default
```

## Topologies

This is intended to be run as standalone.

## Update Strategies

At-once is the recommended update strategy.
