# Rails/Web Supermarket

This package contains the rails/web server application for Supermarket.

## Maintainers

* The Chef Maintainers <humans@chef.io>

## Type of Package

Service package

## Usage

This application requires either a database/redis configuration, or service-groups to bind to.

Example service install / load and setup:

```
hab svc load chef/supermarket --bind database:supermarket-postgresql.default --bind redis:supermarket-redis.default
```

The package hooks will run as `root:root` due to the need to create a symbolic link to the rendered configuration template used for `.env` inside the package, but will exec supermarket using configuration values `app[user|group]` (`supermarket:supermarket` by default). 

## Bindings

The package requires a bind for both postgresql and redis, and will use `bind.sys.ip` to connect to either. In the case where the services are run on the same machine, one must ensure that the configuration for the services allows the supermarket application to connect on this IP address, as this might resolve outside the machine and return. See the READMEs for each individual service for more details.

The package also exports configuration parameters for nginx and the sidekiq worker to make use of. Note that the ssl configuration and external listening ports for nginx are defined here, as the values are also needed to construct a URL for the application.

## Configuration

To start the application at a minimum the `secret_key_base` must be defined, but often we also want to specify the `chef_server_url` and `fqdn` (and `fqnd_sanitized`) . Other common options include for db/redis configuration, OAuth2 for chef-server, S3 configuration, `force-ssl` for nginx, and Github app values.
