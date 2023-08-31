+++
title = "Add Nodes to the Deployment"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Add Nodes to the Deployment"
    parent = "automate/deploy_high_availability/manage_ha_cluster"
    identifier = "automate/deploy_high_availability/manage_ha_cluster/ha_add_nodes_to_the_deployment.md Add Nodes to the Deployment"
    weight = 200
+++

Chef Automate HA comes with five different types of deployment flows. This page tells you how to add more nodes to your deployment processes.

## Add more Nodes to the OnPremises Deployments

In this section, we will see how to add more nodes to the on-premises deployment for all the databases, i.e., Chef Managed, AWS Managed and Customer Managed Database.

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. It needs the IP addresses of the nodes you want to add as comma-separate values with no spaces in between.

For example,

- To add nodes with IP 10.1.2.23 to automate, run the following command:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23
    ```

- To add nodes with IP 10.1.2.23 and 10.0.1.42 to the chef-server, run the following command:

    ```sh
    chef-automate node add --chef-server-ips 10.1.2.23,10.0.1.42
    ```

- To add nodes with IP 10.1.2.23 and 10.0.1.42 to OpenSearch, run the following command:

    ```sh
    chef-automate node add --opensearch-ips 10.1.2.23,10.0.1.42
    ```

- To add nodes with IP 10.1.2.23, 10.0.1.54 and 10.0.1.42 to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --postgresql-ips 10.1.2.23,10.0.1.42,10.0.1.54
    ```

You can mix and match different services to add nodes across various services.

- To add nodes with IP 10.1.2.23 to automate and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23 --postgresql-ips 10.0.1.42,10.0.1.54
    ```

- To add nodes with IP 10.1.2.23 to automate, nodes with IP 10.1.0.36 and 10.0.1.233 to chef-server, and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23 --chef-server-ips 10.1.0.36,10.0.1.233  --postgresql-ips 10.0.1.42,10.0.1.54
    ```

Once the command executes, it will add the supplied nodes to your automate setup. The changes might take a while.

- Make sure to update your loadbalancer configuration with the IP address of the new node. For reference, check [Load Balancer Configuration page](/automate/loadbalancer_configuration/)

{{< note >}}

- If you have patched some external config to any existing services, then apply the same on the new nodes.
For example, if you have patched any external configurations like SAML or LDAP or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, Postgresql, and OpenSearch.
- The new node will be configured with the certificates already configured in your HA setup.
- If you had applied unique certificates per node, then the certificates of one of the nodes have been applied by default on the new nodes.
- If you want to change the certificates for the new nodes, you can manually run the `chef-automate cert-rotate [options]` command.

{{< /note >}}

{{< warning >}}
It's essential to ensure that the IP address of the nodes you are trying to add has sufficient resources and is reachable from the bastion host.
{{< /warning >}}

## Add more Nodes In AWS Deployment with AWS Managed Database

In this section, we will see how to add more nodes to the AWS deployment for AWS managed database.

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. When you run the command, it needs the count of the nodes you want to add as an argument. For example,

- To add two nodes to automate, run the following command:

    ```sh
    chef-automate node add --automate-count 2
    ```

- To add three nodes to the chef-server, run the following command:

    ```sh
    chef-automate node add --chef-server-count 3
    ```

- To add one node to OpenSearch, run the following command:

    ```sh
    chef-automate node add --opensearch-count 1
    ```

- To add two nodes to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --postgresql-count 2
    ```

You can mix and match different services to add nodes across various services.

- To add one node to automate and two nodes to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --automate-count 1 --postgresql-count 2
    ```

- To add one node to automate, two nodes to chef-server, and two nodes to PostgreSQL, run the following command:

    ```sh
    chef-automate node add --automate-count 1 --chef-server-count 2 --postgresql-count 2
    ```

Once the command executes, it will add the supplied nodes to your automated setup. The changes might take a while.

{{< note >}}

- If you have patched some external config to any existing services, apply the same on the new nodes. For example, if you have patched any external configurations like SAML or LDAP or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, Postgresql, and OpenSearch.
- The new node will be configured with the certificates already configured in your HA setup.
{{< /note >}}

{{< warning >}}
Downgrading the number of instance_count for the backend nodes will result in data loss. We do not recommend downgrading the backend nodes.
{{< /warning >}}

## Add more nodes In AWS Deployment with Chef Managed Database

In this section, we will see how to add more nodes to the AWS deployment for Chef managed database.

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. When you run the command, it needs the count of the nodes you want to add as an argument. For example,

- To add two nodes to automate, run the following command:

    ```sh
    chef-automate node add --automate-count 2
    ```

- To add three nodes to the chef-server, run the following command:

    ```sh
        chef-automate node add --chef-server-count 3
    ```
