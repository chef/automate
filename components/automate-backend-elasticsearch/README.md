# Automate-HA-Backend-Elasticsearch

This is a wrapper package of core/elasticsearch for Chef Automate that provides a backend HA Elasticsearch.

## Maintainers

The Habitat Maintainers humans@habitat.sh

## Type of Package

This is a service package.

## Notes

This package wraps the upstream Elasticsearch package. It installs and clusters
Elasticsearch. It also loads in a basic dashboard based on the contents of `files/dashboard.tar.gz`. If you want to incorporate dashboard changes, simply deploy this app, make your changes in kibana, then run `files/exporter.rb`. This will export a new dashboard.tar.gz containing all local kibana dashboards that you can drop in this package.
