# Nginx

This package wraps the [Chef Base Plans Nginx Package](https://github.com/chef-base-plans/nginx). More documentation can be found in the original package repository's [README](https://github.com/chef-base-plans/nginx#readme) and the [Nginx documentation](https://www.nginx.org/docs/).

This document is to be used in addition to the previous two documents and will not cover Nginx configurations or the package specifics in depth (such as bindings, topologies, update strategies), but will focus on the usage of the application package in the Supermarket context.

## Maintainers

* The Chef Maintainers <humans@chef.io>

## Type of Package

Service package

## Usage

This plan should be used by starting the service with:

```
$ hab [svc load|start] chef/supermarket-nginx --bind rails:supermarket.default
```

Note: It requires a binding to the rails (web) application.

The package hooks will run as the `root:root` but will make use of `worker[user|group]` configuration values (`supermarket:supermarket` by default) for the worker processes.

## Bindings

The application provides no exports but requires a bind to the rails web server providing the port the server is listening on, the ports nginx should listen on, the SSL switch, as well as the FQDN. Note that FQDN comes in two properties as a sanitized version is required for the redirect configuration, but habitat configuration templating does not provide a way to modify values.

## Configuration

Every available configuration item is given a reasonable default. Generally a user should only have to modify `ssl` configurations as the service port (i.e. the port the rails application is listening on) is pulled in through the bind - as well as the ports nginx itself will listen on. This seems a bit of an anti-pattern but due to the need for the rails application itself to be aware of the particular port to build URLs to itself, this allows this configuration value to be set once and propagated for the application and the proxy. 

Most of the configuration files are directly copied from the Chef Base Plans repository with additional values/configurations inserted for Supermarket specific defaults or to allow for additional items to be configured that were not configurable in the original package (such as log directory).
