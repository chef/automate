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
