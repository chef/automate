# automate-HA-pgleaderchk

This pkg runs a HTTP server that reponds with a 200 if the local PostgreSQL service is a Leader.

## Maintainers

The Habitat Maintainers humans@habitat.sh

## Type of Package

This is a service package.

## Build

In order to build the pkg, you must enter the studio at the root: `automate`

## Bindings

This pkg expects to bind to PostgreSQL as `database` in order to use `port`, `superuser_name` and `superuser_password`

## Usage

example:
```bash
hab svc load chef/automate-ha-pgleaderchk --bind database:automate-ha-postgresql.default
```

## Topologies

This is intended to be run as standalone.

## Update Strategies

At-once is the recommended udate strategy.
