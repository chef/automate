# Automate 2 & TLS

## Overview
Service to service communication in A2 should be secure, meaning traffic is both encrypted and authenticated. The approach taken is to use mutual TLS for all service to service communication.

## Mutual TLS
Mutual TLS is an extension of the basic TLS we normally use. When using TLS, the client receives a certificate from the server when connecting to it. It checks this certificate against its list of trusted CAs. If the certificate is signed by a trusted CA, the client will check the common name or SANs to make sure it is talking to the server it expects. In this exchange, only one party does any sort of authentication, and that is the client. With mutual TLS, the client provides its certificate to the server as well as the server providing its certificate to the client. Now both parties can check the certificates they received to ensure they are signed by a trusted CA.

## A2 Setup

### Overview
For A2, the deployment service manages its own CA. This CA is used to create certificates for each service. Each certificate is assigned a common name that is the same as its component name. Certificate key pairs are distributed to each service, along with the deployment service's CA certificate.

When Service A wants to talk to Service B, Service A will provide Service B with its certificate. Service B will provide Service A with its certificate. Service A's job is to check that Service B's certificate is signed by the deployment service's CA. It also should check that the common name is Service B. Service B will check that Service A's certificate is signed by the deployment service. If both Service A and Service B verify that the certificates are correct, they may continue with their exchange.

### CA Management
The deployment service uses [certstrap][3] to manage its CA. Each Automate deployment receives its own CA with a name that contains a deployment Id.

### Habitat
The deployment service renders config files for each service. These files are templates, and will be provided 3 variables to render templates: `SSLKey`, `SSLCert`, and `SSLRootCert`. Once rendered, these config files are applied using `hab config apply`. 

Below is an example config that is used for the license-control-service and can be found [here][4]:

```toml
license_contents = "{{.LicenseControlConfig.License}}"

[service]
host = "0.0.0.0"
port = 10124

[tls]
key_contents = """{{.SSLKey}}"""
cert_contents = """{{.SSLCert}}"""
root_cert_contents = """{{.SSLRootCert}}"""

[mlsa]
accept = true
```

Note: These config values are not currently transmitted to each service in a secure way. This work is tracked by [DEPLOY-5][1].

## A2 Development
Below are some tools that may be helpful when adding new services.

### Generating Dev Certificates
Certificates for the development environment can be generated inside the hab studio:

```bash
> generate_dev_service_cert component-name
```

It's important that you use the component name based on the directory name in the components directory. This is a convention. You can use a different name if you absolutely need in an exceptional case.

Once the service certs are generated, they will to be added to the .envrc file so that they get picked up by docker-compose as well as the habitat dev config.

### Go GRPC Helpers
The [automate-grpc][2] component contains a helper library for creating secure GRPC servers in the A2 environment, as well as connecting to those GRPC servers. When connecting to a server, you must  specify the component name of the server that you expect.

### Examples
The deployment team has converted the existing services to use TLS. You can use the following PRs as examples for how to set it up:

- #571
- #564
- #556
- #553
- #551
- #548
- #544

## Potential Issues

### No Mutual TLS Support
There are some tools people are using for development that don't seem to support mutual TLS. For such cases, replacements for those tools will need to be found.

grpc_cli is one example. grpcurl is a potential replacement.

### Hostname Verification
Implementations generally assume using the domain name for validating the common name. Our setup makes no assumptions about domain name, and thus relies on our convention of using the component for the common name.

This problem seems to manifest in a few places. First, dev tools people use. For these cases, there is usually a way to disable hostname verification. For example, grpcurl has a `-insecure` flag. It's important to note that unless the tool specifies, `-insecure` means no validation will happen and the only thing that is true is the channel will be encrypted. Some tools also provide a way to either set the expected name, or something equivalent to that. curl has a `--resolve` flag which lets you specify what IP address a name resolves to.

This problem also seems to resolve in certain services we use. In this case, skipping the hostname verification check is ok as long as there is still a step that checks the certificate is signed by our CA. Skipping all checks is not acceptable.


[1]: https://chefio.atlassian.net/browse/DEPLOY-5
[2]: https://github.com/chef/automate/tree/master/components/automate-grpc
[3]: https://github.com/square/certstrap
[4]: https://github.com/chef/automate/blob/master/components/automate-deployment/data/config/license-control-service.toml
