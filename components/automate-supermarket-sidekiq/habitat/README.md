# Sidekiq

This package contains the sidekiq worker for Supermarket.

## Maintainers

* The Chef Maintainers <humans@chef.io>

## Type of Package

Service package

## Usage

This application requires either a database/redis configuration, or service-groups to bind to, as well as a required bind to the web/rails application.

Example service install / load and setup:

```
hab svc load chef/supermarket-sidekiq --bind redis:supermarket-redis.default --bind database:supermarket-postgresql.default --bind rails:supermarket.default
```

The package hooks will run as `root:root` due to the need to create a symbolic link to the rendered configuration template used for `.env` inside the package, but will exec supermarket using configuration values `app[user|group]` (`supermarket:supermarket` by default).

## Bindings

See the rails/server application for details on the postgresql/redis binds as they are identical in this case.

The required bind to supermarket allows Sidekiq to talk to the application and optionally Fieri when enabled.

## Configuration

To start the application at a minimum the `secret_key_base` must be defined. Other common options include for db/redis configuration, and Github app values.
