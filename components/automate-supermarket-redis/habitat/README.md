# Redis

This package wraps the [Core Plans Redis Package](https://github.com/habitat-sh/core-plans/tree/main/redis). More documentation can be found in the original package's [README](https://github.com/habitat-sh/core-plans/tree/main/redis#readme) and the [Redis documentation](https://redis.io/documentation).

This document is to be used in addition to the previous two documents and will not cover Redis configurations or the package specifics in depth (such as bindings), but will focus on the usage of the application package in the Supermarket context. 

## Maintainers

* The Chef Maintainers <humans@chef.io>

## Type of Package

Service package

## Usage

Redis first requires configuration to setup the credentials for protected mode. No defaults are assumed for users of the package.

Example service install / load and setup:

```
hab pkg install chef/supermarket-redis

echo '
requirepass="password"
' | hab config apply supermarket-redis.default $(date +%s)

hab svc load chef/supermarket-redis
```

And either binding rails/sidekiq to it (see "Binding" below) or providing the necessary parameters to the dependent services.

The package hooks will run as `supermarket:supermarket` as defined in `pkg_svc_user` and `pkg_svc_group` defined in `plan.sh`.

## Bindings

Redis provides the bind `port` and `requirepass` for other services to bind to.

```
hab svc load <ident> --bind redis:supermarket-redis.default
```

Note when using the bind the package will attempt to connect to redis using `bind.redis.first.sys.ip` which resolves to the public IP of the machine gathered by the Habitat Supervisor. This then has two requirements to work properly:

1. The `bind` array must contain an interface listening for external traffic, (i.e. _not just_ the default loopback at `127.0.0.1`).
2. The machine must be able to receive traffic on the given address (see #1) and port. In the case of an AWS machine, for example, the security group must allow traffic on the specified port.

Note these apply even if the application is loaded on the same machine, as it will reach out publicly through the IP. For example, in the case of AWS, the security group should allow traffic on the specified port set to itself.

If desired, this can be modified in the future to check for the existence of host configuration value even if the bind exists. This may not be desired in all circumstances, so is not currently included.

More information regarding binds can be found in the Core Plans documentation mentioned in the initial blurb.

## Configuration

All the configuration values currently have sensible defaults. The configuration file was directly copied from the original package repository with additional values/configurations inserted for Supermarket specific defaults or to allow for additional items to be configured that were not configurable in the original package (such as data directory).
