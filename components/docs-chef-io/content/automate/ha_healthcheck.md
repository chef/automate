+++
title = "Health Check Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Health Check Commands"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_healthcheck.md Health Check Commands"
    weight = 90
+++

{{< warning >}}
{{% automate/ha-warn %}}s
{{< /warning >}}

This page includes commands that can be executed for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

## Automate HA Service Commands

- Get the Automate HA cluster Information 
  
```cmd
    chef-automate info 
```

- Post Deployment, run smoke test cases on Automate HA cluster, please run the command from bation node
  
```cmd
    chef-automate test --full 
```

- Validate the cluster but skip "chef-automate diagnostic run" when performing the smoke tests
  
  ```cmd
    chef-automate test  
  ```

- Run the smoke test on specific cluster 
  
  ```cmd
    chef-automate test automate
    chef-automate test chef_server
    chef-automate test opensearch
    chef-automate test postgresql 
  ```

- To get the status of the cluster, run the command from bastion node. 
  
  ```cmd
    chef-automate status 
  ```

- Check the service status on frontend nodes (Automate), ssh to the frontend node.

```cmd
    chef-automate ssh --hostname a2
    chef-automate status
```

- Check the service status on frontend nodes (Chef Infra Server), ssh to the frontend node.

```cmd
    chef-automate ssh --hostname cs
    chef-automate status
```

- Check the service status on backend nodes (Postgres nodes), ssh to the backend node.
  
```cmd
   chef-automate ssh --hostname pg
   hab svc status
```

- Check the service status on backend nodes (Opensearch nodes), ssh to the backend node.
  
```cmd
    chef-automate ssh --hostname os
    hab svc status
```

- Patch a config to the Front end nodes (Automate)
  - create a config file `automate.toml`
  
``` cmd
    chef-automate config patch automate.toml --automate
```

- Patch a config to the Front end nodes (Chef Server)
  - create a config file `chefserver.toml`
  
``` cmd
    chef-automate config patch chefserver.toml --chef_server
```

- Patch a config to the Back end nodes (Open Search)
  - create a config file `opensearch.toml`

``` cmd
    chef-automate config patch opensearch.toml --opensearch
```

- Patch a config to the Back end nodes (Open Search)
  - create a config file `postgresql.toml`

``` cmd
    chef-automate config patch postgresql.toml --postgresql
```

- Collect the Gatherlogs for Automate HA cluster,run the command from bastion node.  
  - logs are collected at `/var/tmp`

```cmd
    chef-automate gather-logs
```

{{< note >}}

- Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node
    of the cluser.
- After patching some services will go restart. So the health status will take upto 2 minutes to show healthy.

{{< /note >}}

{{< warning >}}

- For certificate rotation, don't use `config patch`. Instead `cert-rotate` command can be used. To know more about certificate rotation click [here](/automate/ha_cert_rotaion)
- While patching the same from **the provision host**, structures such as TLS from OpenSearch configuration toml file and SSL from PostgreQL configuration toml file will be ignored.

{{< /warning >}}

- View the active Habitat gossiped toml config for any locally loaded service:
  - ssh to the backend opensearch nodes `chef-automate ssh --hostname os`

```cmd
    source /hab/sup/default/SystemdEnvironmentFile.sh
    automate-backend-ctl show --svc=automate-ha-opensearch
```

- ssh to the backend postgres nodes `chef-automate ssh --hostname pg`

```cmd
    source /hab/sup/default/SystemdEnvironmentFile.sh
    automate-backend-ctl show --svc=automate-ha-postgresql
```
