# Automate Deployment

The deployment-service and automate-cli are responsible for the
installation and management of Chef Automate. This includes:

- initial installation
- Automate 1 to Automate 2 migrations
- configuration changes
- automatic and manual Automate 2 upgrades
- backup and restore
- system diagnostics

This directory contains the deployment-service -- a long running
service that provides gRPC APIs to accomplish the above tasks.

The [automate-cli](../automate-cli) component contains the
chef-automate command which provides the user interface to the
deployment-service's capabilities.

# Development

## Development Environments

For most day-to-day development, you can use the Habitat studio-based
development environment described in our [developer
documentation](../../dev-docs/DEV_ENVIRONMENT)

Most new development features and test should be designed to work in
the studio if possible.

We also try to ensure that the build, unit tests, and linter work from
outside the studio, using the Makefile directly:

- `make build`
- `make unit`
- `make lint`
- `make fmt`

Because the deployment-service is responsible for OS setup and
bootstrapping, we also have specialized development environments
described in a section below.

## Testing

New features and bug fixes require tests. We are not dogmatic about
the kind of test you should write. Write a test that:

- Will break if the feature breaks;
- Can be integrated into CI; and
- Can be run by developers with a level of effort proportional to the
  confidence the test gives.

We try to test at 2 levels:

- "unit" tests: `make unit`

  These tests are written using the standard Go testing tools. You can
  run them with `make unit` from the current directory. While we
  currently have substantial mocking in our unit test, our goal is to
  reduce and avoid mocking over time. Unit tests that talk to
  databases and filesystems are absolutely fine provided they run
  quickly and reliably.

- Integration tests

  Because deployment-service is focused on interactions with the
  underlying operating system and Habitat service supervisor,
  integration tests are our gold standard for whether a feature works.

  We have an [integration test framework](../../integration) that runs a
  complete A2 installation inside a docker container. From there, we
  write tests in Go using the [`chef-automate
  diagnostics`](../automate-cli/pkg/diagnostics) command or directly
  in Bash.

## Specialized Development Environment

### Vagrant based deployment-service development environment

Because the deployment-service must deal with the underlying operating
system configuration, we have custom development environments in
addition to the Habitat studio development.

The vagrant environment automatically mounts the automate directory to
`/a2`.

This allows you to deploy and test locally build artifacts in an
environment more similar to the systems customers will be using. To
get started:

```
host> # build any required components for testing via the habitat studio
host> cd A2_ROOT/components/automate-deployment
host> make linux
host> vagrant up
host> vagrant ssh
vagrant> sudo -i
vagrant> cd /a2/components/automate-deployment
vagrant> make run
```

### A1 to A2 test environments

Most A1 to A2 migration development and testing can be done in the
studio or the vagrant-based test environment using automate-cli's
`--self-test` flag:

```
chef-automate upgrade-from-v1 --self-test
```

In some cases, it is necessary to test the real migration code paths
without the mock interfaces used by self-test. To facilitate this the
`a1migration` directory has a docker-based test environment suited for
that purpose.  See the README in that directory for more details.
