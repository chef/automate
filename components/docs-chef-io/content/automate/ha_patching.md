+++
title = "Overview"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Overview"
    identifier = "automate/deploy_high_availability/patching/ha_patching.md Overview Of Patching Configuration"
    parent = "automate/deploy_high_availability/patching"
    weight = 200
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

Patching new configuration in all nodes can be done from bastion server. This page explains all the argument and flags available with `config patch` command

{{< warning >}}

-   For Opensearch and Postgresql make sure to run `get-config` command to get the applied configuration and add the config to be patched on top of it.  
    {{< /warning >}}

Flags available

-   --frontend - "Patch toml configuration to the all frontend nodes"
-   --fe - "Patch toml configuration to the all frontend nodes"
-   --opensearch, -o - "Patch toml configuration to the opensearch node"
-   --postgresql, -p - "Patch toml configuration to the postgresql node"

Usage:

chef-automate config patch --< FLAG > < PATH_TO_TOML_FILE >

Example: chef-automate config patch --frontend /home/frontend.toml

{{< note >}}

Frontend patch will be applied to all nodes where are Postgresql and OpenSearch changes will be applied to only one node of the cluser.

{{< /note >}}

### Get applied configuration from Opensearch and Postgresql

Following command will get the applied configuration in backend nodes (Opensearch and Postgresql) and put it into a toml file (Default name: output_config.toml)

Usage:

chef-automate config patch -get-config <REMOTE_TYPE> --file <OPTION_TOML_FILE>

Example: chef-automate config patch --get-config [opensearch/os/postgresql/ps]

#### Flags and respective arguments

-   -F, --file string File path to write the config (default "output_config.toml") (default "output_config.toml")
-   -G, --get-config Get applied config from Opensearch or Postgresql nodes

##### Arguments available for `get-config` flag

-   `os` or `opensearch` for opensearch node
-   `pg` or `postgresql` for postgresql node
