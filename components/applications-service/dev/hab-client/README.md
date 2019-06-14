# Hab Client End to End Docker Environment

## Short Version

1. If you don't have Automate running in the studio, enter the studio and
   `start_all_services`
2. In the studio, get an API token with `get_api_token`
3. Until the habitat supervisor supports NATS TLS, you need to turn off TLS on
   the automate event-gateway. Use the following toml and enable it with
   `chef-automate config patch` (not setting this will cause an error like
   `Could not establish streaming connection to NATS server -> timed out
   waiting on channel`):
   ```
   [event_gateway]
     [event_gateway.v1]
       [event_gateway.v1.sys]
         [event_gateway.v1.sys.service]
           disable_frontend_tls = true
   ```   
4. Outside of the studio, run the script: `./run-hab-sup.sh -t YOUR_TOKEN`

## Description

The Dockerfile in this directory defines a docker image with a real habitat
client that you can connect to your Automate development environment to test
the EAS Applications functionality with a real Habitat supervisor. It's
designed to run on MacOS and uses a Docker for Mac-specific feature.


