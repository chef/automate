# Third Party Services

This is a list of all the third party services in use as part of the Automate 2.0 platform. This is only the pieces that are run as a service and not the various libraries, etc.

## Services

| Service       | Component Path         | Habitat Package             | Wrapped Habitat Package |
| ------------- | ---------------------- | --------------------------- | ----------------------- |
| Elasticsearch | automate-elasticsearch | chef/automate-elasticsearch | core/elasticsearch      |
| Postgresql    | automate-postgresql    | chef/automate-postgresql    | core/postgresql         |
| Nginx         | automate-load-balancer | chef/automate-load-balancer |core/nginx               |
| Nginx         | automate-ui            | chef/automate-ui            | core/nginx              |
| Dex           | automate-dex           | chef/automate-dex           | core/dex                |

## General Notes

* As a rule, all logging should be done to stdout/stderr in lieu of writing to a file.
* All third-party services should be wrapped and not be used directly. For example, we use `chef/automate-postgres` instead of `core/postgres`.
