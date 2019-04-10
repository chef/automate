# Automate 2 License Control Service

In this repository we are defining a small server in Go that will run within
the Automate 2 system and provide policy information to the rest of the system
derived from a given license file.

## Development

### Using the Habitat Studio
The hab studio is setup to work on the `license-control-service`. It can build,
lint, and vet the code.

```shell
$: hab studio enter

# build it
studio> build components/license-control-service

# start it
studio> start_license_control_service

# test it
studio> test_license_control_service
```

### Local development

In the components directory for the service you can run the make commands:

```shell
# Setup and install dependencies
make setup

# Run tests for sanity check
make test

# Build a License Control Service binary
make build

# Compile and run the License Control Service
make run

# Start and background the server with a config file
./license-control-service serve --config config.dev.toml  &

# Start and background the server with environment variables
AUTOMATE_LICENSE_CONTROL_CERT_PATH="../../dev/certs/license-control-service.crt" \
AUTOMATE_LICENSE_CONTROL_KEY_PATH="../../dev/certs/license-control-service.key" \
AUTOMATE_LICENSE_CONTROL_ROOT_CA_PATH="../../dev/certs/Chef_Automate_FAKE_Dev.crt" \
AUTOMATE_LICENSE_CONTROL_LICENSE_TOKEN="$(cat ../../dev/license.jwt)" \
./license-control-service serve &

# stop the server
killall license-control-service
```

#### Running the demo client

The Demo client connects to the local running server which has the license in memory.

```shell
go run integration/demo_client.go \
  -update_license $(cat ../../dev/license.jwt) \
  -cert ../../dev/certs/license-control-service.crt \
  -key ../../dev/certs/license-control-service.key \
  -root_cert ../../dev/certs/Chef_Automate_FAKE_Dev.crt
```
