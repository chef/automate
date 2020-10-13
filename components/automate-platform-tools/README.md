# Platform Tools

Platform tools is a collection of command line utilities that Automate
services can use in their run-scripts for common tasks:

- pg-helper: A tool that exposes common postgresql bootstrapping
  operations (creating users, creating databases, etc). This tool can
  be used in `init` and `run` scripts for services that need
  postgresql databases set up.

- service-health: A tool to make a health check GRPC call to a given
  service and return an exit code compatible with Habitat's
  health_check hook.

- secrets-helper: A tool for managing on-disk shared-data. This is to
  support legacy applications. It is called secrets-helper because it
  provides data using the same API that is expected by the Erlang
  chef_secrets library. It does NOT handle secrets securely and is
  simply intended for giving processes that expect data via the
  chef_secrets interface access to on-disk data. New services should
  instead use the secrets-service for managing user-provided secrets.
