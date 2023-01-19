+++
title = "Automate HA Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Automate HA Commands"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_healthcheck.md Automate HA Commands"
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

- Post Deployment, run the smoke test cases on Automate HA cluster and run the command from the bastion node.
  
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

- To get the status of the cluster, run the command from the bastion node.
  
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

```cmd
    chef-automate gather-logs
```

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
