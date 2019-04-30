# workflow-ctl

workflow-ctl is a ruby command used for managing
automate-workflow. This code previously lived in the Automate 1
repository and it is in the process of being cleaned up.

## Usage

In development use `./bin/workflow-ctl`. On a running system it will
be just `workflow-ctl`.

Run with `--help` to see usage instructions.

## Development

After checking out the repo, run `make setup` to install dependencies. Then,
run `make test` to run the tests.

The RSpec unit tests are in the spec directory. The specs in the
spec/integration directory test the functionality of the commands, but all
configuration is mocked so it can run in any envionment.

## Functional Tests

The functional tests for this have not been updated to account for the
new home for workflow-ctl
