+++
title = "HA Health Check Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA Health Check Commands"
    parent = "automate/install"
    identifier = "automate/install/ha_healthcheck.md HA Health Check Commands"
    weight = 230
+++

This page includes commands you can execute for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

## Automate HA Service Commands

- Get the Automate HA cluster Information

```cmd
  chef-automate info 
```

- Post Deployment, smoke test cases on Automate HA cluster, run the command from bastion node:

```cmd
  chef-automate test --full 
```

- Validate the cluster but skip “chef-automate diagnostic run” when performing the smoke tests

```cmd
  chef-automate test  
```

- Run the smoke test on a specific cluster

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

- Patch a config to the Front end nodes 
  - add the config to the location `/hab/a2_deploy_workspace/config/<automate.toml>`

``` cmd
  chef-automate config patch /hab/a2_deploy_workspace/config/automate.toml
```

- Collect the Gatherlogs for Automate HA cluster, run the command from bastion node.

    - logs are collected at `/var/tmp`

```cmd
  chef-automate gather-logs
```
