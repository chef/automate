# How To Add a New Service to Automate 2

Adding a new service to Automate 2 is not currently as easy as we would like. This document will guide you through it. There are two general chunks of work required when adding a new service:
* You need to setup your service to fulfil the platform contract
* You need to teach deployment service how to deploy your service

## Fulfilling the Platform Contract

### mTLS

Services that implement some functionality for other services via gRPC will need a TLS certificate, key, and certificate authority certificate to integrate with A2's mutual-TLS auth system. Create the following files in `habitat/config`:

* `root_ca.crt`: `{{~ cfg.tls.root_cert_contents ~}}`
* `service.crt`: `{{~ cfg.tls.cert_contents ~}}`
* `service.key`: `{{~ cfg.tls.key_contents ~}}`

Deployment service will automatically create these for you when needed. Configuring your service to use these will be dependent on the service; where possible, you should configure your service for these via the secrets helper: https://github.com/chef/automate/tree/master/components/automate-platform-tools/cmd/secrets-helper

### Config (which implies protobuf)

Configurations are defined via protocol buffers. To get this working, you'll need a place to put your config `.proto` file and some corresponding go code (even if your service is a wrapper of a 3rd party package or not written in go).

The path for this code isn't standardized, but if you don't have a reason to do otherwise, put the code in `pkg/api`. Within that directory, you'll need a `config_request.proto` file and a `config_request.go` file. When you compile your `.proto` file, the compiler will put the result in `config_request.pb.go`.

A basic `.proto` file is like this:

```
// -*- mode: protobuf; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-
syntax = "proto3";

package chef.automate.infra.esgateway;
option go_package = "github.com/chef/automate/components/NAME_OF_SERVICE/pkg/api";
import "api/config/shared/global.proto";
import "api/config/shared/tls.proto";
import "components/automate-grpc/protoc-gen-a2-config/api/a2conf/annotations.proto";
import "google/protobuf/wrappers.proto";

message ConfigRequest {
	option (chef.automate.api.service_config) = {name: "NAME_OF_SERVICE"};

	V1 v1 = 1;

	message V1 {
		System sys = 1;
		Service svc = 2;

		message System {
			chef.automate.infra.config.Mlsa mlsa = 1;
			chef.automate.infra.config.TLSCredentials tls = 2;

    	Service service = 3;
			Log log = 4;

			message Service {
				google.protobuf.StringValue host = 1;
				google.protobuf.Int32Value port = 2 [ (chef.automate.api.port) = { name: "service", default: YOUR_DEFAULT_PORT, protocol: "grpc" } ];
			}

			message Log {
				google.protobuf.StringValue level = 1;
			}
		} // System

		message Service {}
	} // V1
}
```

Our tooling for compiling protobufs in the habitat studio assumes a script at the path `scripts/grpc.sh`. You can figure out some of the boilerplate by looking at examples in existing services, but basically this script needs to run the following command somewhere:

```
  protoc -I .\
    -I "$GOPATH/src" \
    --go_out=plugins=grpc:"$GOPATH/src" \
    --a2-config_out=$GOPATH/src \
    "${proto_files}"
```

If you want to run the protobuf compiler via `make`, you can use this make target:

```
proto:
	cd ../../ && hab studio run 'source .studiorc; compile_go_protobuf_component YOUR_SERVICE_NAME'

.PHONY: proto
```

The `config_request.go` file should implement the following functions (where `ac` is `github.com/chef/automate/api/config/shared`):
* `NewConfigRequest() *ConfigRequest`: return a config request struct with zero values for everything but no nil pointers
* `DefaultConfigRequest() *ConfigRequest`: fill in the config request with all the default values
* `(c *ConfigRequest) Validate() error`: return an error for invalid config
* `(c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error)`: generally used to copy the mTLS credentials into your configuration.
* `(c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig)`: pull in global config values that are relevant to your service.

A minimal `PrepareSystemConfig` looks like:

```
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds
	return c.V1.Sys, nil
}
```

A minimal `SetGlobalConfig` looks like:

```
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = ac.GlobalLogLevelToZapLevel(logLevel)
	}
}
```

Note that handling of log level is dependent on your logging code: zap and logrus are slightly different for go, and other things will probably be different from them. See https://github.com/chef/automate/blob/master/api/config/shared/config.go for more.

Once you have the basics working for your config code, then you need to integrate it with the combined automate configuration system. To do that, you first need to import your config protobuf into the combined `AutomateConfig` protobuf, which is defined in `components/automate-deployment/pkg/api/automate_config.proto`. Once done, you will need to run `make proto` in the `automate-deployment` component.

### Backup Spec

Every service needs to inform the deployment service how it wishes to be backed up, even if stateless. The file that defines these things is `components/automate-deployment/pkg/backup/spec.go`. For a stateless service, modify the `DefaultSpecs(chefServerEnabled bool, workflowEnabled bool) []Spec` function with something like this:

```
{Name: "YOUR_SERVICE_NAME", WriteMetadata: false},
```

## Teaching Deployment Service How to Deploy Your Service

### Expeditor Bldr Config

Expeditor is part of the build system, not deployment service, but you
will need to configure it anyway to make the build system build and
publish your package. It reads the bldr configuration file when
determining what packages to build. Add your package to the
`HABITAT_PACKAGES` file in the root directory an run:

    go run ./tools/bldr-config-gen


### Manifest Chicken and Egg Issue

"The manifest" is a JSON file which is created by the script `.expeditor/create-manifest.rb`. The manifest contains exact versions of habitat packages that comprise an Automate release. The package versions are determined by querying the habitat depot/bldr for the versions of every package described in the `product.meta` file (more on that below). The reason we must query the depot is because we do not build every package on every build, so instead we build the manifest based on what exists in the depot. During tests we rely on the deployment service preferring packages from local disk to upgrade just those packages with changes.

When introducing a new service, the manifest is a problem because there are initially zero version of your new package in the depot, so a query for the current version of that package will fail and the manifest can't be generated. One way around this is to merge your new service in two pull requests. The first just needs to have a buildable package so that there's something in the depot to query. If you've followed this guide in order, you should be able to submit and merge your first pull request now and then follow up with a second pull request which includes the work described below (along with whatever other test suites, etc. you need).

Note: deployment-team has added some code that makes tests use a locally-generated manifest. This should allow you to add a service in one pull request. But this code hasn't remained stable so far, as it only gets fully exercised when new services are added. You can try to add your service in one pull request, but you may need to debug some test setup.

### Recompiling Binds

You need to run `make generate` from `components/automate-deployment`
whenever you add/change/remove bindings. You don't necessarily need to
change the bindings when first adding your service, but when you start
to wire up your new service to other services this will be needed.

### The product.meta File

The list of services and other mandatory packages that comprise Automate 2 and other Chef products is maintained in `product.meta`. This file is also used to generate the manifest file that represents a Chef Automate 2 release as described above. Edit this file and add your service. Services may also define extra metadata for their service in a `package.meta` for their component. See https://godoc.org/github.com/chef/automate/lib/product for more information.
