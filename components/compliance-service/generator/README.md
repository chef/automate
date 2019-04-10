# Compliance reports generator

The goal of this tool is to fetch sample data from existing nodes. This sample data is used to extrapolate more data.

## Pre-condition

You need to start the `client_containers` before running this generator.

```bash
$ make start-client-containers
```

Profiles need to be cached. We download the [compliance-profiles](https://github.com/chef/compliance-profiles) into `./tmp/profiles`.

```bash
$ make download-sample-profiles

# Use the downloaded profiles to scan the machines(containers) and store the sample reports in `report/samples/`
$ make sample

# docker containers can be removed now that we have the sample reports
$ make stop-client-containers

## Usage

```bash
# Using the sample reports, generate final InSpec reports(number of nodes defined in `matrix.yml`(simulation->nodes))
$ make generate
# unless the target is changed in `reports_dest.yml`, final generated reports will be saved in `report/nodes/`

# estimate data size
$ make stats
```
