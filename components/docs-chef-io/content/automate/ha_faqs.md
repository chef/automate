+++
title = "FAQs"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "FAQs"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_faqs.md HA FAQs"
    weight = 130
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

This page explains the frequently encountered issues in Chef Automate High Availability (HA) feature and the steps to resolve them. In addition, this page also lists the health check commands.

## Frequently Asked Questions

### What are different ways to provision an on-premise deployment?
- There are two types of infrastructure to provision on-premise deployment as follows:
   -  Existing Infrastructure
   -  Existing Cloud Infrastructure
      -  AWS
      -  Google Cloud Platform

### What are different types of backup available for deployment on GCP platform?
- Google Cloud Storage(GCS) and File System(FS) type backup is supported for deployment on GCP platform

### How to check logs For automate nodes?
- To view the logs please do ssh to the respective node by running the command from bastion node 
`./chef-automate ssh --hostname a2`

- choose the instance based on the output. To view the logs run the command 
`journalctl --follow --unit chef-automate` 

### How to check logs For Chef Infra Server nodes?
- To view the logs, please do ssh to the respective node by running the command from bastion node 
`./chef-automate ssh --hostname cs`

- choose the instance based on the output. To view the logs run the command 
`journalctl --follow --unit chef-automate` 

### How to check logs For Postgres nodes?
- To view the logs, please do ssh to the respective node by running the command from bastion node 
`./chef-automate ssh --hostname pg`

- choose the instance based on the output. To view the logs run the command 
`journalctl --follow --unit hab-sup` 

### How to check logs For Opensearch nodes?
- To view the logs, please do ssh to the respective node by running the command from bastion node 
`./chef-automate ssh --hostname os`

- choose the instance based on the output. To view the logs run the command 
`journalctl --follow --unit hab-sup` 

### How to Pass the custom config for the Frontend node (Automate / ChefInfraServer)?
- Create a file with say `customconfig.toml`, pass the absolute path `config_file=/ABSOLUTE_PATH/customconfig.toml`



### How to Add more nodes In AWS Deployment, post deployment.
The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. It needs the count of the nodes you want to add as as argument when you run the command.

For example,

- if you want to add 2 nodes to automate, you have to run:

    ```sh
    chef-automate node add --automate-count 2
    ```

- If you want to add 3 nodes to chef-server, you have to run:

    ```sh
    chef-automate node add --chef-server-count 3
    ```

- If you want to add 1 node to OpenSearch, you have to run:

    ```sh
    chef-automate node add --opensearch-count 1
    ```

- If you want to add 2 nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --postgresql-count 2
    ```

You can mix and match different services if you want to add nodes across various services.

- If you want to add 1 node to automate and 2 nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-count 1 --postgresql-count 2
    ```

- If you want to add 1 node to automate, 2 nodes to chef-server, and 2 nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-count 1 --chef-server-count 2 --postgresql-count 2
    ```

Once the command executes, it will add the supplied number of nodes to your automate setup. The changes might take a while.

{{< note >}}

- If you have patched some external config to any of the existing services then make sure you apply the same on the new nodes as well.
For example, if you have patched any external configurations like SAML or LDAP, or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, PostgreSQL, and OpenSearch.
- The new node will be configured with the certificates which were already configured in your HA setup.

{{< /note >}}


{{< warning >}}
  Downgrading the number of instance_count for the backend nodes will result in data loss. We do not recommend downgrading the backend nodes.
{{< /warning >}}

### Is Automate HA supports unencrypted traffic with managed service like AWS-Opensearch / RDS?
 - No, Automate HA support https connection only with Managed services. 


### How to check logs while doing backup or restore?

Set *log-level* debug using the command `chef-automate debug set-log-level deployment-service debug` and execute *journalctl* command, `journalctl --follow --unit chef-automate`.


### How to perform infrastructure cleanup for AutomateHA nodes

Execute the following commands from bastion host as per your deployment to perform infrastructure cleanup

for AWS deployment
```bash
chef-automate cleanup --aws-deployment
```

for on premises deployment
```bash
chef-automate cleanup --onprem-deployment
```

## HA Health Check Commands

This section includes commands that you can execute for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

### Log Check Commands

The Chef Automate frontend and backend nodes service logs are available via `journalctl` from each node. You can identify the service by the name in the generated output preceding with the log line.

- Execute the following command, `journalctl --follow --unit hab-sup`, to view the backend logs related to all hab services.

Where the *--unit* displays the logs from the specified unit, and *--follow* means to follow the journal.

- Use the *grep* command to filter the logs related to a specific service. For example, run this command `journalctl --follow --unit hab-sup | grep 'automate-ha-opensearch'` to view the log of the habitat component in the Chef Automate frontend node.

- Execute the following command, `journalctl --follow --unit chef-automate`, to view the log of the frontend (chef-automate and chef-server instances) nodes.

- Use the *grep* command to filter the logs for a single service. For example, run this command, `journactl --follow --unit chef-automate | grep ingest.service` to view the ingest logs of the Chef Automate frontend node.

### Health Check Service Commands

- Execute the following command, `chef-automate status`, to SSH the frontend node.

- Execute the following command, `hab svc status`, to SSH the backend node.

- Execute the following command, `hab svc status`, to verify the health of any services on a node.
