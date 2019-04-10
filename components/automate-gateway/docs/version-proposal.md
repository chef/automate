# Gateway API Versioning and Compatibility

The versioning and compatibility approach proposed here is inspired by the Kubernetes project. The pages linked below provide an overview of K8s API, details of their API conventions, and an explanation of their approach to API changes and compatibility.

* [overview of K8s API](https://kubernetes.io/docs/concepts/overview/kubernetes-api/)
* [K8s API Conventions doc](https://github.com/kubernetes/community/blob/master/contributors/devel/api-conventions.md)
* [K8s API Changes doc](https://github.com/kubernetes/community/blob/master/contributors/devel/api_changes.md#alpha-beta-and-stable-versions)

## Versioning

1. Version in URL path and proto/go package path.
2. Ship with `v0` and `v1` so legacy endpoints are separate.
3. Introduce `v1alpha2` to mark API as experimental

### REST API

The REST API is versioned in the resource path. At launch, we will have a single version covering the entire API `/api/vX` . We will support multiple versions at the same time. In the future, we expect to partition the API into groups (see K8s approach). The groups will provide more granular version management that better communicates our approach to evolving the API by adding new versioned endpoints (as opposed to thinking of the entire API as a single cohesive version).

### GRPC API

The GRPC API is versioned in the protobuf package path. Example:
```go
package chef.automate.api.authz.v1;
option go_package = "github.com/chef/automate/components/automate-gateway/api/authz/v1";
```
### Versions available at launch

We will have two versions available at launch: `v0` and `v1`. The `v0` is used for legacy endpoints. Everything else is launched as `v1`.
### Add experimental API version “alpha”

New APIs can be introduced using an alpha version (e.g. `v1alpha2`, `v2alpha1`). When an API endpoint contains “alpha” in its version it is marked as experimental. This means it can change or be removed.
### Recommendation for versions in request and response messages

Following the pattern in the K8s API, I think we should adopt a practice of adding the API version to request and response message using an `api_version` string field. Having the version expressed in message content can help us automatically convert between versions.

## Compatibility

REST and GRPC APIs versioned as `vX` are stable APIs that we commit to support for an on-going period.

### Backwards compatibility

A `vX` API will only be changed in a backwards compatible manner. A change is compatible if:
1. Adds new functionality that is not required.
2. Does not change existing semantics. No change in default value or behavior.
3. Does not change interpretation of fields or values
4. Validation does not get more strict.
5. Consider a request body serialized before your change, and then sent after the change. Does the system still work and respond as expected?
6. A single property cannot be represented using multiple fields.

### Breaking changes always are made with a new version
When an API needs to change, we do so using a new version, likely introduced first using the alpha experimental label.

