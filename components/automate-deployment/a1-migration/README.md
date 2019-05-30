# A1 Migration

This bit of tooling is intended to provide the primitives and helpers for
testing domain service and full end-to-end A1 to A2 data and config migrations.

# Constraints

* Domain service developers must be able to use the A2 dev studio to test
  migrations of their service.
* The `automate-deployment` developers must be able to provision an A1 install
  with data to test end-to-end migrations.
* CI systems should be able to use the same studio helpers and installs to
  test domain services and end-to-end migrations.
* The system should allow creating and migrating arbitrary A1 versions and
  different data sets to A2, though the initial focus will be on A1 stable to
  A2 current.
* Aside from initial test artifact/package creation, it should be fast and easy
  and test all migrations.

# Philosophy

The data layer and application code layer of an A1 install will be broken
into two separate pieces. Breaking them apart will allow us to create data
artifacts that can be used to test domain services in the hab studio
without requiring a running A1 install and doing a full migration. It'll also
give us the ability to create several data sets for testing happy and sad path
migration scenarios.

The data artifacts will contain all the A1 state that would be included in a
backup, including:
 * Postgresql Data
 * Elasticsearch Data
 * Compliance Profiles
 * Notifications Data
 * Configuration Data (`/etc/delivery`)

The `a1-migration-data` artifact will be a bundle of this data with a no-op
process that copies the data from the artifact into the proper A1 staging area
and/or installation area.

The `a1-migration` service will be a container or VM which runs the A1 omnibus
package.  It will serve two purposes: First, it will be used to create
`a1-migration-data` data artifacts. Developers that wish to create them can
start the `a1-migration` service, populate or modify the data, and create data
artifacts. Second, the `a1-migration` service can be used to test end-to-end
migrations.

# Usage

## Building Migration Data Artifacts
Migration data artifacts, by their nature, will probably be different each time
they are created because we'd want to add a new test case. Therefore, the approach
that we've taken is agnostic enough to allow a user to hand craft a test
case in the `a1-migration` container before creating the artifact.

For the two CI test environments (the verify pipeline and the
a1migration nightlies) we have two data packages that we use:

- a1-data-migration-minimal (used by verify pipeline)
- a1-data-migration-full (used by nightlies)

### Creating a1-data-migration-full

This package contains the data produced by the setup.rb script and ES2
indexes. To recreate the package:

```
HAB_ORIGIN=chef make a1-migration-up
HAB_ORIGIN=chef make a1-migration-create-full-artifact
# make sure you only have 1 artifact in a1-migration/results/
HAB_ORIGIN=chef make a1-migration-upload-artifact
```

### Creating a1-data-migration-minimal

This package has the data produced by the setup.rb script and a
pre-dumped postgresql data file. To recreate this package:

```
HAB_ORIGIN=chef make a1-migration-up
HAB_ORIGIN=chef make a1-migration-create-minimal-artifact
# make sure you only have 1 artifact in a1-migration/results/
HAB_ORIGIN=chef make a1-migration-upload-artifact
```

### Creating your OWN data package. How fun!

To build a migration data artifact you first have to provision a functioning
`a1-migration` service. In most cases, using the published base container will
be sufficient to get started, however if you need to make changes to the base
container you can modify the install and setup scripts and run
`make a1-migration-build` to build a new docker container.

Start the `a1-migration` service container: `make a1-migration-up`

After the container is up you can run `make a1-migration-shell` to gain shell
access to install. The Automate UI should also be available on 0.0.0.0:443.

Make the required changes to the A1 data.

### Creating and Uploading Artifacts

If you wish to publish artifacts to the depot, you must first:
1. Obtain the hab private key for the `devchef` origin. It's in lastpass
2. Download the public keys for `devchef`: `hab origin key download devchef`
3. Get a hab auth token and set it as an env variable: https://www.habitat.sh/docs/using-builder/#generate-an-access-token
4. Have release engineering add you to the origin in the hab depot (see
   `#releng-support` slack channel)

Now create a hab data artifact: `make a1-migration-create-artifact`
Optionally upload: `make a1-migration-upload-artifact`

If you rebuild the data artifact used in the nightly a1-migration test, you will need to
update `A1_BUILDER_PASSWORD` and `AUTOMATE_API_DEFAULT_PASSWORD` in `automate/scripts/nightly_migration.sh`
with the credentials generated during `make a1-migration-up` when you built the data artifact. The generated creds are in
`automate/components/automate-deployment/a1-migration/keys/enterprise-test-admin-login-creds`.


```
NOTE: If you have troubles with docker make sure you increase the resources available
to the docker hyperkit VM. At least 4GB of memory is suggested.
```

### Using Migration Data Artifacts in the A2 dev studio

Data artifacts can be used for end-to-end testing scenarios and domain service
testing in the hab studio.

Start the published migration package in the studio: `hab sup start chef/a1-migration-data`

TODO: None of these helpers really exist yet. As the migration tooling becomes
more real these helpers will be added.

#### Functions
`prepare_a1_test_data_migration()` # prepare the latest from the `HAB_ORIGIN`
`prepare_a1_test_data_migration($HAB_ORIGIN/$data-version)`
`prepare_a1_test_data_migration(/path/to/data.hart)`

`prepare_a1_test_migration()` # starts the latest from the `HAB_ORIGIN`
`prepare_a1_test_migration($HAB_ORIGIN/$app-version $HAB_ORIGIN/$data-version)`
`prepare_a1_test_migration(/path/to/app.hart /path/to/data.hart)`

If they wish to restart a migration environment they simply run
`stop_a1_test_migration`
`start_a1_test_migration ...`

#### Testing End-to-End Migrations

End-to-end tests run in a docker container which has a1 and Chef Server
installed on it. Due to limitations of systemd when used in a docker
container, the docker image has a1 and Chef Server packages installed
but both need to be `reconfigure`'d after starting the container.

The basic steps to start the environment are:

```
yourlaptop$ make a1-migration-up
yourlaptop$ make a1-migration-shell
dockerhost$ /a1-migration/run.sh
```

The `a1-migration` directory in the repo will be mounted in the
container, so you can run `cp bin/linux/chef-automate a1-migration` to
copy a build of the `chef-automate` CLI into the container.

Optionally, load a test data artifact:

TODO: Make this a make target
```shell
make a1-migration-shell
delivery-ctl stop
chef-server-ctl stop
hab pkg start chef/a1-migration-data
delivery-ctl start
delivery-ctl reconfigure
chef-server-ctl start
chef-server-ctl reconfigure
```

## CI

After these primitives are available and we have promoted artifacts to the
CI origin we could run migration scenarios with the same hab artifacts.
