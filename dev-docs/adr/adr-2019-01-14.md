# ADR 2019-01-14 APIs live together in a top level folder

## Status

* ACCEPTED (2019-01-14)

## Context

In A2, we use protobufs to describe our APIs and config. Services talk to each other using clients generated from these protobuf files. The `deployment-service` uses the protobufs for config to allow users to configure A2.

While this works, it has a few flaws. First, there is no clean separation between a service's implementation and its API. This means that when certain services update a service, but not the API, dependent services must be rebuilt. A similar thing is true for config, where `deployment-service` ends up depending on all services, and chaining any of them causes `deployment-service` to be rebuilt.

## Decision

We will define APIs and config in our top-level `api` directory. This top-level `api` directory will have 3 subdirectories:

* external - Our public facing API, which is currently exposed by `automate-gateway`
* internal - API definitions for each of our services
* config - The config definitions for the services and the global config definition

Services that want to communicate with each other should do so by using the clients that are in the `internal` API subdirectory. When taking such a dependency, the service must update the `.expeditor/config.yml` file with this information. Components which are services should not have dependencies to other components which are services in the `.expeditor/config.yml` file.
