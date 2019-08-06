# Compliance Service

## Getting Started

- Our all-in-one service (the same one we ship):<br>
    run `make run-with-es-pg` from here (components/compliance-service)

This all-in-one service includes reporting, scanner, and inspec-agent, which can each
be run individually is so desired....but we recommend just running the whole shabang. Making great things here people, don't want you to miss out on alllll the compliance amazingness!! <br>

_Notes:_<br>
 - Running the all-in-one service will start grpc on port 10121<br>

    _Something gone wrong?  Take a look in `/tmp/compliance.log` to view the logs for the service._<br>

 - Should you wish to view the log output live in your terminal, you can instead run `make start-es-pg && make run-with-logs`.  This is useful when debugging.<br>


## Development Information

### Development

Make sure you have Go installed!
    https://golang.org/doc/install?download=go1.9.1.darwin-amd64.pkg

Clone this repo to a logical place in your $GOPATH, i.e. `~/go/src/github.com/chef/automate/components/compliance-service`

Make some edits and run the binary with the commands provided in the Makefile. Tada!


### Dependency Management

We use https://github.com/golang/dep for dependency management.
```
brew install dep
```

**The vendor folder for A2 is checked in to the repo; please refer to `a2/docs/development.md`**

### Sample Data

To load sample data for the reporting (ElasticSearch based service):
    ```
    make ingest-reports-into-es
        _please note: this will send in three reports from 2017. You will need to adjust your calendar section in the ui to view this data_
    ```

To load sample market profiles:
    ```
    make download-sample-market-profiles
    ```

To send a sample report to the ingest service:
    ```
    make send-ingest-report
    ```

Three nodes will be ingested:
 * "debian(1)-zeta-linux(f)-apache(s)-failed" - scan: 2018-02-09
 * "windows(1)-zeta-apache(s)-skipped"        - scan: 2018-03-04
 * "centos(2)-beta-nginx(p)-apache(s)-passed" - scan: 2018-03-04, 2018-03-05

following this node name convention:
```
platform(number_of_total_scans)-environment-profile(status)-profile(status)-overall_status
```


Use the scanner tests to load a bunch of data, and reports generated from jobs:<br>
    `make start-ssh-node && make test-scanner NO_DELETE="true"` <- this will give you now-time scan reports. <br>

Login to your cc_pg (postgres container): `make login-pg`
Clear your postgres data: `make clear-pg`
Clear your elastic data: `make clear-es`

### Testing

The integration tests for the compliance service are written using ruby minitest. All tests and test-support files can be found in `compliance-service/api/tests`.

----
*What do we run in CI? `make test-integration-reporting`, `make test-db`, `make test-integration-scanner`, as well as the unit tests, `make fmt` and `make test-unit`*

*To run the smoke tests, `inspec exec inspec/a2-api-integration --sudo --target ssh://USERNAME@a2-local-inplace-upgrade-dev.cd.chef.co -i KEY` from root of a2, using inspec version ~1.51*

----

To run reporting, scanner, or nodemanager tests,
 - `make kill-all`
 - `make test-prep`
 - `make run-with-logs`
 - in a new window, `make test-reporting`, `make test-scanner`, or `make test-nodemanager`.

*Easiest way to run the tests: `make run-test`.  This will also do all the setup stuff/run the service for you. Great for set and forget*
*Easiest way to run partial tests: `TEST="04_stats*" make run-test`

Some notes:
* `make kill-all` is a good thing to run first, all the time
* `make test-prep` will get all the things ready; it will run:
   - `make download-sample-market-profiles` to get the sample profiles needed for testing
   - `make start-ssh-node` to start the ssh node we test against in 40_jobs_ssh_spec
   - `gem install inspec` to get latest inspec.
* `make test-reporting` will always start by ingesting some reports. You will see the service re-index afterwards. This is normal.
* `make test-scanner` will test scanner including scheduler and ingest connection.
* `make test-nodemanager` will run the ingest to manager connection test and then all the nodemanager tests.

Test ElasticSearch migrations:
A1_DATA=true   TEST="60_A1_migration_spec.rb"   make test-automate-upgrade
A2V1_DATA=true TEST="61_A2V1_migration_spec.rb" make test-automate-upgrade
A2V2_DATA=true TEST="62_A2V2_migration_spec.rb" make test-automate-upgrade
A2V3_DATA=true TEST="63_A2V3_migration_spec.rb" make test-automate-upgrade

_When in doubt, run the same commands we run in CI_

You will also need the following two env variables:
 - AWS_ACCESS_KEY_ID
 - AWS_SECRET_ACCESS_KEY
to run the aws-related-tests.  You may already have IAM creds in ~/.aws/credentials, or you can create new IAM creds from the AWS UI.

To run the azure-related tests, you'll need the following env vars:
 - AZURE_CLIENT_ID
 - AZURE_CLIENT_SECRET
 - AZURE_TENANT_ID
These credentials can be found in LastPass under "inspec azure test creds".  The azure tests will automatically get skipped if you do not have these env vars set.

When the test command is run, the ruby grpc code is generated and the GRPC function is called against that with the request (see https://github.com/chef/automate/components/compliance-service/blob/master/api/tests/run.rb#L316.

Note: if you ever run into a problem while running the tests (or you simply need to debug a test you're writing), `require 'pry';binding.pry` is your best friend! Stick that right before the expectation that is failing to fall into a pry breakpoint and see what's going on.

Note: trouble running the tests because the ruby grpc bits are bombing out? run `go get -u github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway`

The unit tests are run by executing `make test-unit-reporting` and `make-test-unit-scanner`.
The db tests are run by executing `make test-db`.

There are many other wonderful testing targets, please take a look at testing section of the Makefile to see them all!

### GRPC/Protobuf

Whenever you make a change to a `*.proto` file, you'll need to run `make proto` from the root of this repo to generate the updated code.

### DB (Postgres)

We are using https://github.com/golang-migrate/migrate for migration. It happens in-code, triggered from the `dao/pgdb/db.go`. The code for the migration bits is in `lib/db/migrator`.
To make a change to the schema, create an `up` and `down` file in `dao/pgdb/migration/sql` following the established pattern.

## Repo structure and services information
### Repo Structure

Here's a quick overview of what's going on in this repo:

 - Start with `cmd/compliance-service/main.go`.
    That calls `cmd.RootCmd.Execute()`, which initializes our cobra cli app (`cmd/compliance-service/cmd/run.go`).

 - The cobra cli app sets up all the flags we need to run the app.
    It initializes our db connection to postgres, and calls out to `automate.Serve` (in the root of the repo, `compliance.go`) with the db instance, configuration options, and grpc binding info.

  - The content in `server.go` sets the es backend and initializes our grpc routes. All grpc routes can be found in `compliance-service/api`.

### Services Information

- reporting: what is it and why?

    Reporting is our elasticsearch based service.  It reports on the state of your infrastructure based on the InSpec reports sent in (for automate 1.x, that's via data-collector, for automate 2.0, that's via ingest-service).  This includes `api/stats`, `api/reporting`, `api/profiles`.

- scanner: what is it and why?

    Scanner is our postgres based service.  It provides the user with the ability to schedule InSpec jobs on specified nodes.  This includes `api/secrets`, `api/nodes`, `api/jobs`.  The expected user flow is: user adds a secret, user adds a node (with a secret id for reference), user adds a job, specifying the desired nodes and profiles to be used for that job.  That job information is then sent over to inspec-agent for execution.

- manager: what is it and why?

    Please refer to https://github.com/chef/automate/blob/master/components/compliance-service/manager/README.md for more information.

- inspec-agent: what is it and why?

    The inspec-agent takes job information and executes it.  It initializes a worker pool based on the provided values for JobBufferSize (max num of jobs that can be accepted in worker queue) and JobWorkers (max num of workers to run in parallel). The service exposes a `jobs` endpoint for retrieving information on job status, and a `workers` endpoint for information on worker status, i.e. `./inspec-agent workers`.


## A2 Build
### Testing things in the whole A2 Environment

From the root of A2, `hab studio enter`. Run `start_deployment_service`, followed by `chef-automate dev deployinate`. Run `build components/compliance-service` to get a local build of the compliance service. Anything not explicitly built will be pulled from the depot.
Run `generate_supertoken` to get a token for curling the api.
