# Data Lifecycle Service

This services is responsible for triggering data lifecycle interface methods on other services
such as purging old data. The service runs these tasks once a day automatically at some configured
time. The lifecycle methods can also be manually triggered.

## Current Status
* The Purge lifecycle method is available
* The DLS currently requests purge for itself and ingest-service
* Purge can be triggered manually using a GRPC client
* Purge happens once a day, configured for midnight UTC by default

## Adding Manageable Services
The list of services to be managed is provided in the toml configuration file which
can be supplied to the `serve` command. A list of these is supplied to the `managed`
map as show below:

```toml
[managed]

  [managed.foo-service]
  address = "localhost:1234"
```

The above example manages `foo-service`, which has implemented the data lifecycle management
interface at `localhost:1234`.


## Running

The service can be started using:

  > make
  > ./bin/data-lifecycle-service serve

Unit and functional tests can be run with:

  > make unit

## Sample Config
```
# The interface to listen on
listen_address = "0.0.0.0"

# The port to to listen on
port = 10190

# What to log (error, info, debug)
log_level = "debug"

# What time to automatically trigger data lifecycle methods
data_run_at = "12:30:00"

# The services the data lifecycle service will manage
[managed]

  [managed.foo-service]
  address = "localhost:1234"
```
