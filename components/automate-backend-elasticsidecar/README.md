# Automate-HA-Elasticsidecar

This is a sidecar service for automate-backend-opensearch that reads users and passwords off the [opensearch] binding and applies them to Opensearch using the odfe tooling.
## Maintainers

The Habitat Maintainers humans@habitat.sh

## Type of Package

This is a service package.

## Notes

This package relies on strict bindings and will not start unless all credentials and ssl are available from Opensearch.

