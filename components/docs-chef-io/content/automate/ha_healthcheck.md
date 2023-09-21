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

- To check the service status on Postgres nodes.
 ```sh 
 chef-automate status --postgresql
 chef-automate status -pg
```

- To check the service status on Opensearch nodes.
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
  - create a config file `frontend.toml`
  
``` cmd
 chef-automate config patch frontend.toml --frontend
```
sorthands for --chef_server is --fe and -f

- Patch a config to the Back end nodes (Open Search)
  - create a config file `opensearch.toml`

``` cmd
 chef-automate config patch opensearch.toml --opensearch
```
sorthands for --opensearch is --os and -o

- Patch a config to the Back end nodes (Postgresql)
  - create a config file `postgresql.toml`

``` cmd
 chef-automate config patch postgresql.toml --postgresql
```
sorthands for --postgresql is --pg and -p

{{< note >}}

- Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node.
- After patching, some services will go restart. So the health status will take up to 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use config patch. Instead, the cert-rotate command can be used. To learn more about certificate rotation, click [here](/automate/ha_cert_rotaion)
- While patching the same from **the provision host**, structures such as TLS from OpenSearch configuration toml file and SSL from PostgreQL configuration toml file will be ignored.

{{< /warning >}}

- Collect the Gatherlogs for Automate HA cluster, and run the command from the bastion node.  
  - logs are collected at `/var/tmp`

```sh
 chef-automate gather-logs
```

- View the active Habitat gossiped toml config for any locally loaded service:
  - ssh to the backend opensearch nodes `chef-automate ssh --hostname os`

```sh
 source /hab/sup/default/SystemdEnvironmentFile.sh
 automate-backend-ctl show --svc=automate-ha-opensearch
```

- ssh to the backend postgres nodes `chef-automate ssh --hostname pg`

```sh
 source /hab/sup/default/SystemdEnvironmentFile.sh
 automate-backend-ctl show --svc=automate-ha-postgresql
```
