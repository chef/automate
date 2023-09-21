+++
title = "Remove Single Node from Cluster"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Remove Single Node from Cluster"
    parent = "automate/deploy_high_availability/manage_ha_cluster"
    identifier = "automate/deploy_high_availability/manage_ha_cluster/ha_remove_single_node_from_cluster.md Remove Single Node from Cluster"
    weight = 210
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Chef Automate HA comes with five different types of deployment flows. This page tells you how to remove nodes from your Automate cluster. Refer to the [Add Nodes to the Deployment](/automate/ha_add_nodes_to_the_deployment/) page to see how to add more nodes to your deployment processes.

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](#replace-node-in-automate-ha-cluster) for reference.
- Removal of nodes for PostgreSQL or OpenSearch is at your own risk and may result in data loss. Consult your database administrator before trying to delete PostgreSQL or OpenSearch nodes.
- Below process can be done for `chef-server` and `automate`.
- Only one node can be removed simultaneously, irrespective of a node type.

{{< /warning >}}

{{< note >}}

- The flags like `opensearch-ip` and `postgresql-ip` are only applicable for the Chef Managed Database cluster

{{< /note >}}

## Remove Single Node From Cluster on On-Premises Deployment

In this section, we will see how to remove single nodes from cluster on the on-premises deployment for all the databases, i.e., Chef Managed, AWS Managed and Customer Managed Database.

The command requires some arguments to determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the IP address of the node you want to remove. For example,

- To remove the Automate node, run the following command:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- To remove the Chef Server node, run the following command:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- To remove the OpenSearch node, run the following command:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- To remove the PostgreSQL node, run the following command:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the particular node from your HA setup. The command might take a while to complete.

- Make sure to remove the IP address of the deleted node from your load balancer configuration. For reference, check [Load Balancer Configuration page](/automate/loadbalancer_configuration/)

{{< note >}}

- The IPs provided must be associated with a node in your HA setup.
- Automate instance count cannot be less than 1.
- Chef Server instance count cannot be less than 1.
- Open search instance count cannot be less than 3.
- PostgreSQL instance count cannot be less than 3.

{{< /note >}}

## Remove Single Node From Cluster on AWS Deployment

In this section, we will see how to remove single nodes from the AWS deployment.

The command requires some arguments to determine the type of node you want to remove from your HA setup from your bastion host. It needs the node's IP address you want to remove as an argument when you run the command. For example,

- To remove Automate node, run the following command:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- To remove a Chef Infra Server node, run the following command:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- To remove OpenSearch node, run the following command:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- To remove PostgreSQL node, run the following command:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command is executed, it will remove the particular node from your HA setup. The command might take a while to complete.
