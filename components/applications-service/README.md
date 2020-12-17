# Applications Service

The applications-service provides backend functionality to aggregate the status
of users' habitat service deployments.

The applications-service itself is responsible for:
- receiving status messages from the event-service via the NATS protocol
- persisting status message to disk via Postgres
- implementing APIs to query the persisted status data

The applications-service relies on functionality implemented in other services:
- `event-service`: Provides a [NATS](https://nats.io) server for internal
  distribution of event messages
- `event-gateway`: Provides a NATS protocol endpoint for external use. External
  connections are subject to authentication checks and must provide a valid
  token.

Note that we are using "embedded" NATS: we consume the NATS codebase as
a normal Go library and then configure it and run it in-process within our own
go application. This mode of operation lets us customize the authentication
logic and use the full range of go TLS configuration settings, which is
required for using mutual TLS.

### What Happens When a Supervisor Connects

Users enable the event stream on the Habitat supervisor by launching the
supervisor with the `--event-stream-*` options. For example:

```
hab sup run \
  --event-stream-application=MY_APP \
  --event-stream-environment=MY_ENV \
  --event-stream-site=MY_SITE \
  --event-stream-url=automate.example.com:4222 \
  --event-stream-token=$token
```

Note that the application and environment properties should ideally be set on
a per-service basis,but this cannot be easily implemented in Habitat currently.

When the supervisor is launched with the event stream enabled, the supervisor
will initiate a connection to Automate over the NATS protocol to the given port
(by default, 4222). The connection is handled by the `event-gateway` service.

In the default (and recommended) configuration, both the supervisor and
Automate will be configured to use encryption. Note that encryption is
negotiated within the NATS protocol, meaning that all connections begin
unencrypted and are then upgraded. One consequence of this fact is that an
external system cannot be used to terminate TLS for NATS.

The `event-gateway` service presents the same TLS credentials as are configured
for Automate's external (HTTPS) load balancer. If Automate's certificate is not
signed by an authority that the supervisor trusts, the
`--event-stream-server-certificate` option is used to make the supervisor trust
it.

After encryption is established, the client presents its access token, which
the event-gateway service verifies with the auth service. The event gateway
also does an authorization check; the token must be permitted for the
`ingest:unifiedEvents:create` action on the `ingest:unifiedEvents` resource.

If all of these checks are successful then the client can emit messages without
further auth checks.

### Event Gateway and Event Service

As described above, the event gateway secures user facing traffic by checking
an authentication token, consistent with how external HTTPS traffic is secured.
Traffic between Automate components is secured with mutual TLS.

In order to support these two authentication regimes, the event gateway is
configured as a NATS gateway. The event gateway will initiate a connection to
the events service, and then the two systems will share NATS routes to allow
messages coming in to the event gateway to reach consumers connected to the
events service.

### Message Format and Message Ingestion

NATS messages are arbitrary byte arrays. We have chosen to use protocol buffers
to encode the messages. The canonical version of the protocol buffer definition
file is in the [habitat project's source tree](https://github.com/habitat-sh/habitat/blob/master/components/sup/protocols/event.proto)
A [shell script is provided](https://github.com/chef/automate/blob/2c815418bc7037fa3d87c2c006c999bcebfbadd1/api/external/habitat/update-event-proto.sh)
to sync updates into the Automate source tree.

NATS provides for scoping messages by publishing them to a "subject." Subjects
can form hierarchies by separating the components of the hierarchy with a dot
character. At this time, the habitat supervisor only sends messages about the
outcomes of service health checks. These are published with the
`habitat.event.healthcheck` subject. The applications service subscribes to all
events in the `habitat.event` hierarchy using the
[wildcard selector](https://docs.nats.io/nats-concepts/subjects).

As messages are received by the NATS client, they are forwarded to a pool of
worker threads, where the protocol buffer message is decoded and processed by
the data layer.

### Information Architecture/Data Model

The primary goal of the applications service is to provide visibility into the
present state of the user's habitat services so the user can detect, diagnose,
and remediate problems quickly. An important subset of this use case is
verifying that a package update correctly propagated to the desired systems and
deployed as expected.

We expect users to have up to 100k services reporting status to Automate. To
support this, we provide aggregations of the services' status as a top-level
concept in the API and UI. Services are primarily grouped by the combination of
package name, habitat service group, application name, and environment name.
Note that we encourage users to not use the habitat level service group as in
most cases it is redundant to the environment name.

Throughout the application, you may find references to the following object
types:
- supervisor: the habitat software on a user's system. The supervisor has
  a unique ID that it generates on its host system. The supervisor can manage
  zero or more services.  
- service: a service managed by the habitat supervisor. A service has name,
  origin, version and release properties, which describe the habitat package
  the service runs. Services have the following properties:
    - origin: the habitat package origin (e.g., core)
    - name: the habitat package name (e.g., redis, nginx, etc.)
    - version: the habitat package version (e.g. 1.0.0)
    - release: the habitat package release slug (e.g., 20201029162531)
    - healthcheck: results of the most recent health check
    - group: the habitat service group name (as set with the `--group` option
      to `hab svc load`). This is different from the service group concept in
      Automate. We encourage users not to use this one.
    - application: the overall application the service belongs to.
    - environment: the environment the service belongs to
    - channel: the builder channel used for package updates
    - update strategy: how the supervisor updates this service within its group
    - disconnected: when no health check updates have been recieved for
      a period of time (default 5 minutes), the service is marked disconnected.
- service group: service groups in Automate represent the collection of all
  services that share the same name, group, application, and environment.
- deployment: a deployment is the collection of all services with the same
  application and environment. This concept mainly pertains to features/views
  that were not ever built, but there may be some references in the source.

In the initial design, service groups, supervisors, and services were modeled
in normalized tables, but this led to executing several queries within
a transaction for each healthcheck message received. To simplify the service
health update code path, we adopted a denormalized schema using Postgresql's
upsert capability (`ON CONFLICT DO UPDATE`). This moved all data
integrity concerns into a well-tested path within the database and removes the
risk of a transaction locking many rows and preventing concurrent updates.

Due to the implementation of the UI's data layer, the API is required to
provide a unique id field for service groups. As service groups are not
represented in the schema, service group IDs are now generated as a hash of the
service group component fields during the ingestion process.

### Package Layout

- config: service configuration data structures
- grpc: starts the gRPC server
- ingester: connects to NATS and handles incoming event stream messages
- nats: wraps the `github.com/nats-io/nats.go` library to provide an
  application-specific client
- params: query parameters for the APIs
- server: top level handlers of RPC methods
- storage: data persistence and retrieval
