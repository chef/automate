+++
title = "Replace Node In Automate HA Cluster"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Replace Node In Automate HA Cluster"
    parent = "automate/deploy_high_availability/manage"
    identifier = "automate/deploy_high_availability/manage/ha_replace_node_in_automate_ha_cluster.md Replace Node In Automate HA Cluster"
    weight = 200
+++

Chef Automate HA comes with five different types of deployment flows. This page tells you how to replace node in Automate HA cluster.

## Replace Node in Automate HA Cluster

- To add a new node, see [Add nodes to the Deployment](/automate/ha_add_nodes_to_the_deployment/) page.
- Stop the Habitat Supervisor on the node .i.e; going to be removed, use the `systemctl stop hab-sup` command to stop the
  habitat supervisor.
- To remove an existing node, see [Remove Single Node from Cluster](/automate/ha_remove_single_node_from_cluster/) page.

## Troubleshooting

### Failure to Replace Nodes

```bash
Error: Upload failed: scp: /var/automate-ha: Permission denied
```

- **Resolution**: Execute the below command.

  ```sh
  cd /hab/a2_deploy_workspace/terraform
  for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep module); do terraform taint $x; done
  cd -
  ```

- Run the `deploy` command again once the module's tainted.

  ```sh
  chef-automate deploy config.toml --airgap-bundle <Path-to-the-airgap-bundle>
  ```
