+++
title = "Patch Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Patch Configuration"
    identifier = "automate/deploy_high_availability/deployment/ha_patching.md Patch Configuration"
    parent = "automate/deploy_high_availability/deployment"
    weight = 7230
+++

Patching new configuration in all nodes can be done from bastion server. This page explains all the argument and flags available using `config patch` command.

The available flags are listed below:

- `--frontend`: Patch toml configuration to the all frontend nodes.
- `--fe`: Patch toml configuration to the all frontend nodes.
- `--opensearch, -o`: Patch toml configuration to the opensearch node.
- `--postgresql, -p`: Patch toml configuration to the postgresql node.

You can use the above patches using the following format:

```toml
chef-automate config patch --< FLAG > < PATH_TO_TOML_FILE >
```

For Example: `chef-automate config patch --frontend /home/frontend.toml`

{{< note >}} Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node of the cluser.{{< /note >}}
