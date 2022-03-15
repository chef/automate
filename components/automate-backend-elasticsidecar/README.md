# Automate-HA-Elasticsidecar

This is a sidecar service for automate-backend-elasticsearch that reads users and passwords off the [elasticsearch] binding and applies them to Elasticsearch using the odfe tooling.
## Maintainers

The Habitat Maintainers humans@habitat.sh

## Type of Package

This is a service package.

## Notes

This package relies on strict bindings and will not start unless all credentials and ssl are available from Elasticsearch.

Just Rebuilding the package to use the chef/elasticsearch-odfe package pull it from the latest stable.
