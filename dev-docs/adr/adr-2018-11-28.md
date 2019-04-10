# ADR 2018-11-28: External Data Services

## Context

We would like to achieve high availability of Chef Automate. HA support for Postgresql & Elasticsearch would require considerable time and effort and likely be very difficult to tailor to individual customer needs. Rather than focus on building out HA support for our backend data services, which do not add much value to the product, we would instead prefer to add support for external data sources. This allows us to focus on building value into the product and allow customers to use existing cloud-based data services or in-house clusters.

## Decision

### Requirements
* Initial deployment of Chef Automate with external data stores should require minimal user configuration.
* Migrations from a single-node install to external data stores should require minimal downtime and operator intervention.
* Backup and Restore operations must work when external data stores are in use.

### External Elasticsearch

#### Deployment
Deployment with external Elasticsearch should remain the same as the existing deployment options, only additional configuration in the initial configuration will be required. Domain services that access Elasticsearch will continue to so by binding to the `automate-elasticsearch` package. When their habitat configuration files are rendered they include the IP address and port. To support external Elasticsearch, we keep all of the existing logic the same, however, when external mode is configured the `automate-elasticsearch` package will not start an instance of Elasticsearch, it’ll create an instance `nginx` which listens on the same address and port but proxies to the upstream external Elasticsearch nodes. This approach has a few properties that are desirable:
  * Existing domain applications do not have to care if the Elasticsearch cluster is local, external, has many nodes or one, or if it requires special signing or security.
  * If the external cluster requires xpack security we can use the configuration they’ve provided, along with `nginx`, to transparently perform mutual TLS with the xpack cert and key.
  * If the external cluster is AWS Elasticsearch Service, we can use the `nginx` proxy to transparently sign outgoing requests with AWS access key and secret key that are provided (or resolved).

#### Migration
There are two ways a user might want to migrate their data from a single instance deployment of Chef Automate to an external cluster.

In cases where the user has control over the cluster configuration, they can join the `automate-elasticsearch` service to their external cluster and configure the automate-elasticsearch node for no shards. As soon a shard relocation has completed the user could stop their Chef Automate installation, remove the `automate-elasticsearch` service from the cluster, and patch their Chef Automate config with the external cluster configuration.

For clusters in AWS's Elasticsearch Service (or Elastic’s Elastic Cloud service), the user would likely need to:
  * Configure their Chef Automate cluster to use S3 backups
  * Create a full backup (which will create S3 snapshot repositories in their desired bucket)
  * Create a new Elasticsearch domain in the cloud providers service
  * Configure their Elasticsearch domain to use the same S3 bucket as the Chef Automate cluster
  * Use the `_snapshot` API of their Elasticsearch domain to restore the latest snapshot of each Chef Automate snapshot repository
  * Configure the Chef Automate install to use their AWS Elasticsearch domain 

#### Backup
Backups should more or less work transparently if the user provides the backup repository type that the `es-sidecar-service` will use when creating the snapshot repositories. If they’re using the AWS Elasticsearch Service we’ll also need the `role_arn` that the repository plugin will assume to take snapshots.

#### Restore
Restoration should work the same, though we might need to add more s3 flags to support the `role_arn`.

### External Postgresql

#### Deployment
Deployment with external Postgresql should remain the same as the existing deployment options. Additional configuration in the initial configuration will be required. Domain services would be accessed in a similar manner to Elasticsearch, using the bind information from the `automate-postgresql` service. When external Postgresql is configured, services could still bind the `automate-postgresql` service, but instead of actually running `postgresql` it would a `postgresql` proxy to then external Postgresql FQDN's. We would configure the proxy to handle any Postgresql authn/authz transparently and require mTLS to the proxy from the upstream clients.

Note: We haven't yet determined what software we'd like to use as the Postgresql proxy, but we've identified a few options that we'll investigate further during implementation and planning.

#### Migration
Migration from an existing Chef Automate install to an external Postgresql cluster can be achieved in several ways depending on target cluster:
 
* The user could create a backup and manually restore the database .sql files that are contained in the backup repository.
* We could build a utility that restores backup databases to a remote Postgresql cluster. We’d have the user create a backup and then restore it to their remote cluster as part of the migration.
* It’s possible that a user could even configure a remote cluster to replicate from the `automate-postgresql` service. The user could wait for replication to catch up, stop Chef Automate, promote a read replica in their cluster to primary, and then update the configuration for external Postgresql.

#### Backup
To support on-premises Postgresql clusters it makes sense to keep the default backup strategy consistent with the current backup implementation, which involves taking a dump of each database and storing the SQL file in a backup repository. RDS/Aurora support creating their own snapshots. We could eventually integrate with this feature for these customers.

#### Restore
Restoration of a database dump would work mostly the same. We’d restore the database SQL against the remote database. If we integrated with RDS/Aurora we could leverage their snapshot restoration ability. RDS supports restoring to an exact point in time. As our backup ID’s are an exact point in time we might be able to utilize such a feature if we decided to build that integration.

## Status

* Proposed and Accepted (2018-11-28)

## Consequences
