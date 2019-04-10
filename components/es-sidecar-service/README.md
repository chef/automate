## About

es-sidecar-service is a service that runs alongside elasticsearch. A single instance is
intended to service multiple ES instances, both to provide coordination of es-related activities across
an ES cluster (and not per-ES-node) and because this is a low-traffic/low-load service.

It provides common ES functionality via a GRPC interface.  At present, this is:

* `PurgeTimeSeriesIndicesByAge`: delete time-series indices older than the provided age
* `PurgeDocumentsFromIndexByAge`: deletion of documents older than provided age from a named index
  * TODO: when compliance is updated to implement purge behavior, it will need to implement this call
    in addition to PurgeTimeSeriesIndicesByAge for compatibility with A1.
    reference: https://github.com/chef/automate/pull/1264/files

This component also provides monitoring of ES node disk usage, and logs alerts when critical thresholds are reached.
This is not yet exposed over GRPC, and no action is taken (beyond logging) when disk usage of any node
exceeds the configured threshold.

## Future Plans
At time of this writing, this service is expected to offer snapshot functionality, and manage multiple
snapshot requests from different domain services such only one is executing at a given time (ES does not support
concurrent snapshots).

## Dev Tools

The primary dev environment for this project is the A2 habitat studio.
`cd` to the repo root and run `hab studio enter`. You can then start up
a test instance with these commands:

```
build components/es-sidecar-service/
start_deployment_service
chef-automate dev deploy-some  YOUR_ORIGIN/es-sidecar-service --with-deps
```

You can run the integration tests in a _clean_ studio with this command:

```
es_sidecar_service_integration
```

This project includes CLI commands to assist in development and local testing.  They're housed in a single file,
`cmd/es-sidecar/commands/dev.go`:

* `./es-sidecar-service grpc` will show the available GRPC calls and allow you to execute them from the shell
* `./es-sidecar-service dev` will show the other development helper tools.

These tools assume a locally running ES5 instance, which you can bring up in docker via `make start-es5`.
You can stop and purge es5 testing data with `make stop-es5` and `make purge-es5` respectively.
