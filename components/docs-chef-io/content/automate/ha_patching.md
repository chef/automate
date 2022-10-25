+++
title = "Patch Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Patch Configuration"
    identifier = "automate/deploy_high_availability/ha_patching.md Patch Configuration"
    parent = "automate/deploy_high_availability"
    weight = 75
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

Patching new configuration in all nodes can be done from bastion server. This page explains all the argument and flags available using `config patch` command.

The available flags are listed below:

-   `--frontend`: Patch toml configuration to the all frontend nodes.
-   `--fe`: Patch toml configuration to the all frontend nodes.
-   `--opensearch, -o`: Patch toml configuration to the opensearch node.
-   `--postgresql, -p`: Patch toml configuration to the postgresql node.

You can use the above patches using the following format:

```toml
chef-automate config patch --< FLAG > < PATH_TO_TOML_FILE >
```

For Example: `chef-automate config patch --frontend /home/frontend.toml`

{{< note >}} Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node of the cluser.{{< /note >}}

## Get Applied Configuration from Opensearch and Postgresql

Following command will get the applied configuration in backend nodes (Opensearch and Postgresql) and put it into a `.toml` file (Default name: `output_config.toml`).

```toml
chef-automate config patch -get-config <REMOTE_TYPE> --file <OPTION_TOML_FILE>
```

For Example: `chef-automate config patch --get-config [opensearch/os/postgresql/ps]`

### Flags and Respective Arguments

-   `-F, --file` string File path to write the config (default "output_config.toml") (default "output_config.toml").
-   `-G, --get-config` Get applied config from Opensearch or Postgresql nodes.

#### Arguments available for `get-config` Flag

-   `os` or `opensearch` for opensearch node.
-   `pg` or `postgresql` for postgresql node.
