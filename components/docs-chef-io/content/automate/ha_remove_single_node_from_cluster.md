+++
title = "Remove Single Node from Cluster"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Remove Single Node from Cluster"
    parent = "automate/deploy_high_availability/manage"
    identifier = "automate/deploy_high_availability/manage/ha_remove_single_node_from_cluster.md Remove Single Node from Cluster"
    weight = 200
+++

## Remove Single Node From Cluster on OnPremises Deployment

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](#replace-node-in-automate-ha-cluster) for reference.
- Removal of nodes for Postgresql or OpenSearch is at your own risk and may result in data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.
- Below process can be done for `chef-server` and `automate`.
- Only one node can be removed simultaneously, irrespective of node type.

{{< /warning >}}

The command requires some arguments to determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the IP address of the node you want to remove.
For example,

- If you want to remove the node of automate, you have to run the:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- If you want to remove the node of the chef-server, you have to run the:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- If you want to remove the node of OpenSearch, you have to run the:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- If you want to remove the node of PostgreSQL, you have to run:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the supplied node from your HA setup. The changes might take a while.

- Make sure to remove the IP address of the deleted node from your loadbalancer configuration. For reference, check [Load Balancer Configuration page](/automate/loadbalancer_configuration/)

{{< note >}}

- The IPs provided must be associated with a node in your HA setup.
- Automate instance count cannot be less than 1.
- Chef Server instance count cannot be less than 1.
- Open search instance count cannot be less than 3.
- Postgresql instance count cannot be less than 3.

{{< /note >}}





## Remove Single Node From Cluster on AWS Deployment

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](/automate/ha_onprim_deployment_procedure/#replace-node-in-automate-ha-cluster) for the reference.
- Removal of nodes for Postgresql or OpenSearch is at your own risk and may result in data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.
- Below process can be done for `chef-server` and `automate`.

{{< /warning >}}

The command requires some arguments to determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the node's IP address you want to remove as an argument when you run the command. For example,

- If you want to remove the node of automate, you have to run the:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- If you want to remove the node of the chef-server, you have to run the:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- If you want to remove the node of OpenSearch, you have to run the:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- If you want to remove the node of PostgreSQL, you have to run the:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the supplied node from your HA setup. The changes might take a while.