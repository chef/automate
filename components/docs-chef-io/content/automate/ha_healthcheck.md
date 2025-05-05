+++
title = "Automate HA Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Automate HA Commands"
    parent = "automate/deploy_high_availability/manage_ha_cluster"
    identifier = "automate/deploy_high_availability/manage_ha_cluster/ha_healthcheck.md Automate HA Commands"
    weight = 240
+++

{{< warning >}}
{{% automate/ha-warn %}}s
{{< /warning >}}

This page includes commands that can be executed for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

## Automate HA Service Commands

- Get the Automate HA cluster Information

```sh
chef-automate info
```

- Post Deployment, run the smoke test cases on Automate HA cluster and run the command from the bastion node.

```sh
chef-automate test --full
```

- Validate the cluster but skip "chef-automate diagnostic run" when performing the smoke tests

```sh
chef-automate test
```

- Run the smoke test on specific cluster

```sh
chef-automate test automate
chef-automate test chef_server
chef-automate test opensearch
chef-automate test postgresql
```

- To get the status of the cluster, run the command from the bastion node.

```sh
chef-automate status
```

- To check the service status on Automate nodes.

```sh
chef-automate status --automate
chef-automate status --a2
```

- To check the service status on Chef Infra Server nodes.

```sh
chef-automate status --chef_server
chef-automate status --cs
```

- To check the service status on PostgreSQL nodes.

```sh
chef-automate status --postgresql
chef-automate status -pg
```

- To check the service status on OpenSearch nodes.

```sh
chef-automate status --opensearch
chef-automate status --os
```

- Patch a config to the Front end nodes (Automate)
  - create a config file `automate.toml`

``` cmd
chef-automate config patch automate.toml --automate
```

sorthands for --automate is --a2 and -a

- Patch a config to the Front end nodes (Chef Server)
  - create a config file `chefserver.toml`

``` cmd
chef-automate config patch chefserver.toml --chef_server
```

sorthands for --chef_server is --cs and -c

- Patch a config to the all Front end nodes (Chef Server + Automate)
  - Create a config file `frontend.toml`

``` cmd
chef-automate config patch frontend.toml --frontend
```

sorthands for --chef_server is --fe and -f

- Patch a config to the Back end nodes (Open Search)
  - Create a config file `opensearch.toml`

``` cmd
chef-automate config patch opensearch.toml --opensearch
```

sorthands for --opensearch is --os and -o

- Patch a config to the Back end nodes (Postgresql)
  - Create a config file `postgresql.toml`

``` cmd
chef-automate config patch postgresql.toml --postgresql
```

sorthands for --postgresql is --pg and -p

{{< note >}}

- Frontend patch will be applied to all nodes where are PostgreSQL and OpenSearch changes will be applied to only one node.
- After patching, some services will go restart. So the health status will take up to 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use config patch. Instead, the cert-rotate command can be used. To learn more about certificate rotation, see [Certificate Rotation](/automate/ha_cert_rotation).
- While patching the same from **the provision host**, structures such as TLS from OpenSearch configuration toml file and SSL from PostgreQL configuration toml file will be ignored.

{{< /warning >}}

- Collect the Gatherlogs for Automate HA cluster, and run the command from the bastion node.
  - Logs are collected at `/var/tmp`

```sh
chef-automate gather-logs
```

- View the active Habitat gossiped toml config for any locally loaded service:
  - SSH to the backend OpenSearch nodes `chef-automate ssh --hostname os`

```sh
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl show --svc=automate-ha-opensearch
```

- SSH to the backend PostgreSQL nodes `chef-automate ssh --hostname pg`

```sh
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl show --svc=automate-ha-postgresql
```

- To Rotate the password for PostgreSQL cluster, run the command from the bastion node

```sh
  cd /hab/a2_deploy_workspace/
  ./scripts/credentials set postgresql --no-auto
```

- To Rotate the password for OpenSearch cluster, run the command from the bastion node

```sh
  cd /hab/a2_deploy_workspace/
  ./scripts/credentials set opensearch --no-auto
```

## Precaution During Backend Node Reboot

For a PostgreSQL cluster, avoid restarting all nodes in quick succession, as this can lead to data loss.
For example, when a follower node (e.g., f1) is rebooted, it begins syncing data from the leader.
If, during this syncing process, the leader node is also restarted, a leader election may occur.
If f1 becomes the new leader before completing its data sync, it may not have the latest data, leading to potential data loss.

## Precaution during opensearch Reboot

- Check cluster health
Run the following commands to ensure the cluster is in a healthy state:

```sh
curl -X GET "https://localhost:9200/_cat/health?v" -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```

```sh
curl -X GET "https://localhost:9200/_cat/recovery?v" -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```

- Disable shard allocation
Before restarting the node, disable shard allocation to avoid unnecessary rebalancing during the restart:

```sh
curl -X PUT "https://localhost:9200/_cluster/settings" -H 'Content-Type: application/json' -d'
{
  "persistent": {
    "cluster.routing.allocation.enable": "primaries"
  }
}' -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```

- Stop indexing and flush the data to disk:

```sh
curl -X POST "https://localhost:9200/_flush" -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```

- Enable shard allocation, Once the node is back up, enable shard allocation:

```sh
curl -X PUT "https://localhost:9200/_cluster/settings" -H 'Content-Type: application/json' -d'
{
  "persistent": {
    "cluster.routing.allocation.enable": null
  }
}' -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```

- Monitor cluster state, Check the cluster health and recovery status to ensure the cluster is stable:

```sh
curl -X GET "https://localhost:9200/_cat/health?v" -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem

curl -X GET "https://localhost:9200/_cat/recovery?v" -k 
--cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem 
--key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem  
--cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem
```