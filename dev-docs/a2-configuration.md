# A2 Configuration

If you want to add a configuration item quickly, see [Adding a new configuration value](#adding-a-new-configuration-value).

Automate 2 services require configuration to run correctly. The
configuration is a combination of user-provided values and dynamically
calculated values. This document describes how configuration values
get from their source to the services that need to consume the values.

This process, pictured below, involves the automate-cli,
deployment-service, and Habitat supervisor and is centered around
protocol buffer (protobuf) descriptions of the configuration.

```text
+---------------+
| User-written  |
| config.toml   |
|               |
+-------+-------+
        |
        v
+-------+-------+    +--------------+     +-------------+     +--------------+     +--------------+
|automate-chef  |    |  Protobuf    |     |  deployment |     |  Habitat     |     |              |
|(automate-cli) +--->+     Message  +---->+   service   +---->+  "user" toml |<----|    hab-sup   |
|               |    |              |     |             |     |              |     |              |
+---------------+    +--------------+     +------+------+     +--------------+     +-------+------+
                                                 |                                         |
                                                 |                               +---------+---------+
                                                 v                               v                   v
                                          +------+------+                 +------+------+     +------+------+
                                          |             |                 |             |     |             |
                                          |    boltdb   |                 |   configs   |     |    hooks    |
                                          |             |                 |             |     |             |
                                          +-------------+                 +-------------+     +-------------+
```

## Configuration Protocol Buffers

Automate 2's configuration is described in a collection of protocol
buffers. These protocol buffers are in `api/config/`.

For each service, we define a ConfigRequest protobuf message that
contains the configuration for that service. For example,
`api/config/teams/config_request.proto` contains the ConfigRequest
message for teams-service:

```protobuf
message ConfigRequest {
	option (chef.automate.api.service_config) = {name: "teams-service"};

	reserved 1 to 2;
	V1 v1 = 3;

	message V1 {
		System sys = 1;
		Service svc = 2;

		message System {
			chef.automate.infra.config.Mlsa mlsa = 1;
			chef.automate.infra.config.TLSCredentials tls = 2;
			Service service = 3;
			Log log = 4;

			message Service {
				google.protobuf.StringValue host = 1 [deprecated=true]; // The listen host is no longer setable(localhost only)
				google.protobuf.Int32Value port = 2 [ (chef.automate.api.port) = { name: "service", default: 10128, protocol: "grpc" } ];
			}

			message Log {
				google.protobuf.StringValue format = 1;
				google.protobuf.StringValue level = 2;
			}
		}

		message Service {
		}
	}
}
```

This message is a typical Automate configuration and has the following
notable properties:

- The `reserved 1 to 2;` is the result of a previous configuration
  format change. Most old services reserve these two field IDs that
  were previously used but are no longer used. New services do not
  need to reserve these field IDs.

- All configuration lives inside a `V1` message. The goal of this was
  to make future large-scale configuration changes possible by
  defining a new `V2` message and supporting both the old and new
  configuration for some period of time. So far, we have not needed a
  new configuration version for any of the services.

- The configuration contains two sections: `System` and `Service`.
  The `System` section represents the majority of the
  configuration. The `Service` section is unused. The original goal of
  the Service section was that it would be dynamic configuration sent
  directly to the relevant service via gRPC calls to the service, but
  it remains unused in nearly every service.

- We include custom `chef.automate.api` options that are used to drive
  code generation. The generated code is used by the
  deployment-service when deploying the service.

- Many fields use wrapper types (for example,
  `google.protobuf.StringValue`). These wrapper types allow us to tell
  the differences between unset, zero-valued configuration items and
  items that the user has explicitly set to the zero-value of that
  type.

These per-service `ConfigRequest` messages are used in the
AutomateConfig message in
`api/config/deployment/automate_config.proto`:

```protobuf
message AutomateConfig {
	reserved 26; // automate-blob-store
	reserved 20; // data-lifecycle-service
	chef.automate.infra.config.GlobalConfig global = 19;
	chef.automate.domain.authn.ConfigRequest auth_n = 1;
	chef.automate.domain.authz.ConfigRequest auth_z = 2;
	chef.automate.domain.compliance.config.ConfigRequest compliance = 10;
	chef.automate.domain.cfgmgmt.ConfigRequest config_mgmt = 6;
	chef.automate.domain.deployment.ConfigRequest deployment = 3;
	chef.automate.domain.dex.ConfigRequest dex = 4;
	chef.automate.infra.elasticsearch.ConfigRequest elasticsearch = 7;
	chef.automate.infra.esgateway.ConfigRequest esgateway = 31;
	chef.automate.infra.es_sidecar.ConfigRequest es_sidecar = 11;
	chef.automate.api.config.ConfigRequest gateway = 5;
	chef.automate.domain.ingest.ConfigRequest ingest = 13;
	chef.automate.infra.load_balancer.ConfigRequest load_balancer = 8;
	chef.automate.domain.local_user.ConfigRequest local_user = 12;
	chef.automate.domain.license_control.ConfigRequest license_control = 16;
	chef.automate.domain.notifications.ConfigRequest notifications = 14;
	chef.automate.infra.postgresql.ConfigRequest postgresql = 15;
	chef.automate.domain.session.ConfigRequest session = 17;
	chef.automate.domain.teams.ConfigRequest teams = 18;
    ...
}
```

This message represents the configuration of the entire system.

After changing any of the configuration protobuf messages, you can
regenerate the Go code generated from the protobufs using the
following command from the development studio:

```shell
compile_go_protobuf_component config
```

## User-provided configuration and CLI interface

Users configure the system by providing a TOML representation of some
or all of the `AutomateConfig` protobuf message which is eventually
passed to the deployment-service via gRPC.

```text
+---------------+
| User-written  |
| config.toml   |
|               |
+-------+-------+
        |
        v
+-------+-------+    +--------------+     +-------------+
|automate-chef  |    |  Protobuf    |     |  deployment |
|(automate-cli) +--->+     Message  +---->+   service   |
|               |    |              |     |             |
+---------------+    +--------------+     +-------------+
```

The user-written TOML file is passed to automate-cli via one of the
following commands:

```shell
chef-automate config patch CONFIG_FILE
chef-automate config set CONFIG_FILE
```

The `patch` command takes a partial configuration and merges it with
the existing configuration. The `set` command takes a complete
configuration and overwrites the existing configuration.

Automate-cli marshals the TOML file into an `AutomateConfig` protobuf
message. The message is then sent to the deployment-service using one
of the following RPCs (corresponding to the two commands above):

```protobuf
rpc PatchAutomateConfig (PatchAutomateConfigRequest) returns (PatchAutomateConfigResponse);
rpc SetAutomateConfig (SetAutomateConfigRequest) returns (SetAutomateConfigResponse);
```

In the TOML representation of protobuf, each TOML section represents a
nested protobuf message and each key-value pair represents the fields
inside that message.

For example, in our config protobufs we have:

```protobuf
// In api/config/deployment/automate_config.proto
message AutomateConfig {
    ...
	chef.automate.domain.teams.ConfigRequest teams = 18;
    ...
}

// In api/config/teams/config_request.proto
message ConfigRequest {
	option (chef.automate.api.service_config) = {name: "teams-service"};

	reserved 1 to 2;
	V1 v1 = 3;

	message V1 {
		System sys = 1;
		Service svc = 2;

		message System {
			chef.automate.infra.config.Mlsa mlsa = 1;
			chef.automate.infra.config.TLSCredentials tls = 2;
			Service service = 3;
			Log log = 4;

            ...
            message Log {
				google.protobuf.StringValue format = 1;
				google.protobuf.StringValue level = 2;
			}
 ...
}
```

Given this definition, in order to set the log-level, the user would
need to provide the following TOML:

```toml
[teams.v1.sys.log]
level = "debug"
```

## Deployment Service

The deployment-service takes the configuration from the user, persists
it to long-term storage, transforms it according to code provided by
the service, and passes it to Habitat for further processing.

```text
 +-------------+     +--------------+
 |  deployment |     |  Habitat     |
 |   service   +---->+  "user" toml |
 +------+------+     +--------------+
        |
        |
        v
 +------+------+
 |             |
 |    boltdb   |
 |             |
 +-------------+
```

### Configuration Mutation Callbacks

In addition to the auto-generated protobuf Go code, we define the
following functions for each service-level configuration object:

- `func DefaultConfigRequest() *ConfigRequest`: Returns the service's
  ConfigRequest struct with any default values set. We define defaults
  here rather than in Habitat's default.toml because it allows us to
  dynamically calculate defaults and works around historical bugs in
  Habitat's configuration processing.

- `func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig)`:
  Takes the GlobalConfig and can set any of the ConfigRequest fields
  based on the global configuration. The GlobalConfig is a section of
  the configuration where the user can set high-level configuration
  options that many services may need to interact with.

- `func (c *ConfigRequest) Validate() error`: Returns an error if any
  of the ConfigRequest's values fail any custom validation logic.

- `func (c *ConfigRequest) ConfigureProduct(*ProductConfig)`: Takes
  the current list of enabled products and can set any of the
  ConfigRequest fields based on those products. Not all configuration
  structs implement this function.

- `func (c *ConfigRequest) PrepareSystemConfig(creds
    *config.TLSCredentials) (config.PreparedSystemConfig, error)`:
    Returns the `System` section of the configuration. It takes the
    `TLSCredentials` and typically copies these credentials into its own
    TLS config.

These functions are defined in `api/config/SERVICE/config_request.go`.

The following diagram shows the process by which deployment-service
mutates a user-provided config into the final configuration. Some of
the process happens at the point that the user sets the configuration
via an RPC call. The process is then finished during the "periodic
converge loop" in the deployment service:

```text
+----------------------------------------------------------------+
|                                                                |
|      +----------------------+     +------------------------+   |
|      |                      |     |                        |   |
|      | DefaultConfigRequest |     |  User-provided Config  |   |
|      |                      |     |                        |   |
|      +----------+-----------+     +------------+-----------+   |
|                 |                              |               |
|                 |      +-------------+         |               |
|                 |      |             |         |               |
|                 +----->+ Deep Merge! +<--------+               |
|                        |             |                         |
|                        +------+------+                         |
|                               |             On Patch/Set Config|
|                               |                                |
|                               v                                |
|                      +--------+---------+                      |
|                      |                  |                      |
|                      | SetGlobalConfig  |                      |
|                      |                  |                      |
|                      +--------+---------+                      |
+-----------------------------  |  ------------------------------+
|                               v                                |
|                      +--------+---------+                      |
|                      |                  |   Every converge loop|
|                      | ConfigureProduct |                      |
|                      |                  |                      |
|                      +--------+---------+                      |
|                               |                                |
|                               v                                |
|                      +--------+---------+                      |
|                      |                  |                      |
|                      |PrepareSystemConfig                      |
|                      |                  |                      |
|                      +--------+---------+                      |
|                               |                                |
|                               v                                |
|                      +--------+---------+                      |
|                      |                  |                      |
|                      |   Final Config   |                      |
|                      |                  |                      |
|                      +------------------+                      |
|                                                                |
+----------------------------------------------------------------+
```

Once processed, the deployment-service writes out the `System` portion
of the `ConfigRequest` struct for each service as TOML to

    /hab/user/SERVICE/config/user.toml

Habitat reads this file and uses it to configure the service.

## Habitat Configuration

```text
 +--------------+     +--------------+
 |  Habitat     |     |              |
 |  "user" toml |<----|    hab-sup   |
 |              |     |              |
 +--------------+     +-------+------+
                              |
                    +---------+---------+
                    v                   v
             +------+------+     +------+------+
             |             |     |             |
             |   configs   |     |    hooks    |
             |             |     |             |
             +-------------+     +-------------+
```

During the deployment-service's periodic converge loop (or during any
converge triggered by user-action) we process the configuration as
described in the previous section and write out the `System` portion
of the `ConfigRequest` struct for each service as TOML to

    /hab/user/SERVICE/config/user.toml

The Habitat supervisor watches this file and re-renders any
configuration templates or hook templates when it changes. The
templates for each service can be found in the repository at:

    components/SERVICE-NAME/habitat/hooks/
    components/SERVICE-NAME/habitat/config/

A2 services use a combination of command line arguments passed via
`run` hooks and configuration files. These files are templated with
the Handlebars templating engine. For example, the `teams-service`
config.yml template includes:

```text
grpc: 127.0.0.1:{{ cfg.service.port }}
log-level: {{ cfg.log.level }}
log-format: {{ cfg.log.format }}
```

The `{{}}` syntax allows for variable interpolation. All of the config
that Habitat knows about is available via the `cfg` structure. Since
only the `System` portion of the service's configuration protobuf is
passed to Habitat, the configuration keys available are those from the
System structure. Thus

```text
cfg.log.level
```

refers to the value the user would set as:

```toml
[teams.v1.sys.log]
level = "debug"
```

and whose default would be set in config_request.go as:

```go
c.V1.Sys.Log.Level = w.String("info")
```

## Adding a new configuration value

1. Add the configuration item to the protobuf definition for the
   service. You should add your item to a protobuf message that is
   part of the `System` portion of the configuration. The protobuf
   definition for the configuration should be in:

        api/config/SERVICE_NAME/config_request.proto

1. Add the default for the configuration option in the
   `DefaultConfigRequest()` function for the service:

        api/config/SERVICE_NAME/config_request.go

1. Rebuild all generated code by running the following in the
   development studio:

        compile_go_protobuf_component config

1. Identify whether the service takes its configuration via
   command-line arguments in the `run` hook, or via a configuration
   file.

1. Modify the appropriate Habitat template to use your new
   configuration value. Remember, the keys available in the template
   are those from the `System` message in the protobuf struct.

1. Rebuild `automate-cli`, `automate-deployment`, and the service
   whose configuration is changing.

## A2 Configuration Protobuf Options

The A2 configuration protobufs include custom field and message
options that drive code generation.

- `chef.automate.api.service_config`: A message option for
  ConfigRequest struct that allows us to specify the name of the
  service that the struct is for.

- `chef.automate.api.port`: A field option for any field that
  specifies a network port used by the service.

## Long-term configuration storage

The deployment-service stores its configuration, along with other
state, in an on-disk boltdb database.

This database is located at

    /hab/svc/deployment-service/data/bolt.db

In normal operation, it is not necessary to manually interact with
this file. However, we've provided some tools to inspect its contents
for debugging purposes:

    hab pkg exec chef/deployment-service deployment-service dumpdb > dbcopy
    hab pkg exec chef/deployment-service deployment-service inspect-db dbcopy

## Document TODO

- Describe how the "platform configuration" and platform scaffolding
  tools work
