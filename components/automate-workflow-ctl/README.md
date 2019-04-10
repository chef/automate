# automate-ctl

This directory currently contains:

* A habitat plan for a "stub" automate-ctl that runs in the habitat-based
  packages and prints a helpful message.
* The ctl-commands used by the automate-ctl that ships with the omnibus-based
  packages.
* The tests for these ctl commands.

The "real" automate-ctl that ships with the omnibus-based packages is created
in the [delivery-ctl software
definition](../omnibus/config/software/delivery-ctl.rb).

This directory will eventually contain everything needed to build, test, and
run automate-ctl for all types of installations.

## Usage

In development use `./bin/automate-ctl`. On a running system it will
be just `automate-ctl`.

Run with `--help` to see usage instructions.

## Development

After checking out the repo, run `make setup` to install dependencies. Then,
run `make test` to run the tests.

The RSpec unit tests are in the spec directory. The specs in the
spec/integration directory test the functionality of the commands, but all
configuration is mocked so it can run in any envionment.

## Functional Tests

The /inspec/automate-ctl-functional directory at the root of this repository
contains the InSpec profile for automate-ctl.  This can be run in the
habitat-based install Test Kitchen enviroment with `kitchen verify functional`
from the cookbooks/automate\_provision directory.

To run against the [omnibus-dev environment](../omnibus/dev/README.md), log in with `make login` from the omnibus/dev directory (after the environment has already been started with `make start`, then run:

    inspec exec /profile

in this environment everything is run as root, so commands that require root
may give incorrect results.
