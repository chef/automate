+++
title = "Chef Infra Server/Chef Backend to Automate HA"
draft = false
gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Che Infra Server/Chef Backend to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_chef_backend_to_automate_ha.md Chef Backend to Automate HA"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}

- Customers using only **Standalone Chef Infra Server** or **Chef Backend** are advised to follow this migration guidance. Customers using **Chef Manage** with Chef Backend should not migrate with this.

- Also, for the customers using a standalone Chef Infra Server, cookbooks should be in the database or S3 but not in the file system.

{{< /warning >}}

This page explains the procedure to migrate the existing Standalone Chef Infra Server or Chef Backend data to the newly deployed Chef Automate HA. This migration involves two steps:

- Backup the data from an existing Chef Infra Server or Chef Backend via `knife-ec-backup`.
- Restore the backed-up data to the newly deployed Chef Automate HA environment via `knife-ec-restore`.

Take a backup using the `knife-ec-backup` utility and move the backup folder to the newly deployed Chef Server. Later, restore using the same utility. The backup migrates all the cookbooks, users, data bags, policies, and organizations. `knife-ec-backup` utility backups and restores the data in an Enterprise Chef Server installation, preserving the data in an intermediate, in editable text format.

{{< note >}}

- The migration procedure is tested on Chef Server versions 14+ and 15+.
- The migration procedure is tested and is possible on the Chef Backend version above 2.1.0.

{{< /note >}}

## Prerequisites

Check the [AWS Deployment Prerequisites](/automate/ha_aws_deployment_prerequisites/#migration) and [On-premises deployment Prerequisites](/automate/ha_on_premises_deployment_prerequisites/#migration)page before migrating.

## Backup the Existing Chef Infra Server or Chef Backend Data

1. Execute the below command to install Habitat:

    ```cmd
        curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh \ | sudo bash
    ```

2. Execute the below command to install the habitat package for `knife-ec-backup`

    ```cmd
        hab pkg install chef/knife-ec-backup
    ```

3. Execute the below command to generate a knife tidy server report to examine the stale node, data, etc.

    ```cmd
        hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s <chef server URL> -u <pivotal> -k <path of pivotal>
    ```

    Where

    - `pivotal` is the name of the user
    - `path of pivotal` is where the user's pem file is stored.
    - `node-threshold NUM_DAYS` is the maximum number of days since the last checking before a node is considered stale.
    For Example:

    ```cmd
        hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s https://chef.io -u pivotal -k /etc/opscode/pivotal.pem
    ```

4. Execute the below command to initiate a backup of your Chef Server data.

    ```cmd
        hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s <chef server url>
    ```

    In this command:

    - `--with-user-sql` is required to handle user passwords and ensure user-specific association groups are not duplicated.
    - `--with-key-sql` is to handle cases where customers have users with multiple pem keys associated with their user or clients. The current chef-server API only dumps the default key. Sometimes, users will generate and assign additional keys to give additional users access to an account but still be able to lock them out later without removing everyone's access.

    For example:

    ```cmd
        hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s https://chef.io`.
    ```

    - Execute the below command to clean unused data from reports. This is an optional step

    ```bash
        hab pkg exec chef/knife-ec-backup knife tidy server clean --backup-path /path/to/an-ec-backup
    ```

5. Execute the below command to copy the backup directory to the Automate HA Chef Server.

    ```cmd
        scp -i /path/to/key /path/to/backup-file user@host:/home/user
    ```

    If your HA Chef Server is in a private subnet, scp backup file to bastion and then to Chef Server.

## Add S3 Configurations for Cookbook Storage

Before restoring the backup on the Automate HA Chef Server, configure [S3 storage](/automate/chef_infra_external_cookbooks_in_chef_automate/) for cookbooks. The cookbooks stored in S3 in the Chef server can be stored in S3 or Postgres in Automate HA.

{{< note >}}

- If you use the same bucket for Automate HA, the old files won't be affected even if the new files for cookbooks are uploaded.
- When you use the new bucket for Automate HA, new files for cookbooks are uploaded.

{{< /note >}}

## Restore Data to Chef Automate HA

- Execute the below command to install the habitat package for `knife-ec-backup`

    ```cmd
    hab pkg install chef/knife-ec-backup
    ```

- Execute the below command to restore the backup.

    ```cmd
    export LC_ALL=en_US.UTF-8
    export LANG=en_US.UTF-8
    hab pkg exec chef/knife-ec-backup knife ec restore <path/to/directory/backup_file> -yes --concurrency 1 --webui-key /hab/svc/automate-cs-oc-erchef/data/webui\_priv.pem --purge -c /hab/pkgs/chef/chef-server-ctl/*/*/omnibus-ctl/spec/fixtures/pivotal.rb
    ```

## Steps to Validate if Migration is Successful

- Download the validation script using below

    ```cmd
        curl https://raw.githubusercontent.com/chef/automate/main/dev/infra_server_objects_count_collector.sh -o infra_server_objects_count_collector.sh
    ```

    Where:

    - `-S` is the Chef Server URL
    - `-K` is the path of pivotal.pem file
    - `-F` is the path to store the output file

- Execute the below command to get the counts of objects

  ```cmd
      bash infra_server_objects_count_collector.sh -S <chef-serve-url> -K /path/to/key -F Filename
  ```

- Repeat the above commands for the new server to get the counts
- Now run the below command to check the differences between the old and new data. Ideally, there should be no differences if the migration was done successfully.

    ```cmd
        diff old_server_file new_server_file
    ```

## In-place Migration (Chef Backend to Automate HA)

As part of this scenario, the customer will migrate from the chef-backend (5 machines) to Automate HA in place, i.e., Automate HA will be deployed in those five machines only where Chef-backend is running. One extra bastion node will be required to manage the deployment of Automate HA on the chef backend infrastructure.

{{< note >}} In-place Migration will require downtime, so plan accordingly. A reduced performance should be expected with this. {{< /note >}}

{{< note >}}

- Set up your workstation based on the newly created Automate-HA's chef-server. It is only needed if you have set up the workstation earlier.
- This in-place migration works when cookbooks are stored in a database or s3. This does not support use-case, where cookbooks are stored in the filesystem.

{{< /note >}}

- To validate the In-place migration, run the validation script before starting the backup and restore.

```cmd
    curl https://raw.githubusercontent.com/chef/automate/main/dev/infra_server_objects_count_collector.sh -o infra_server_objects_count_collector.sh
```

Where:

- `-S` is the Chef Server URL
- `-K` is the path of pivotal.pem file
- `-F` is the path to store the output file

```bash
    bash infra_server_objects_count_collector.sh -S <chef-serve-url> -K /path/to/key -F Filename
```

## Steps for In-place Migration

1. [Backup the existing chef server data](/automate/ha_chef_backend_to_automate_ha/##backup-the-existing-chef-infra-server-or-chef-backend-data)
2. ssh to all the backend nodes of chef-backend and run

    ```cmd
        chef-backend-ctl stop
    ```

3. ssh to all frontend nodes of chef-backend and run

    ```cmd
        chef-server-ctl stop
    ```

4. Create one bastion machine under the same network space.
5. ssh to bastion machine and download chef-automate cli and extract the downloaded zip file

    ```cmd
        https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
    ```

6. Create an airgap bundle using the command

    ```cmd
        ./chef-automate airgap bundle create
    ```

7. Generate the `config.toml` file using the following command:

    ```cmd
        ./chef-automate init-config-ha existing_infra
    ```

8. Edit `config.toml` and add the following:
    - Update the `instance_count`
    - fqdn : load balance URL, which points to the frontend node.
    - keys : ssh username and private keys
    - Ensure to provide Chef backend's frontend server IPs for Automate HA Chef Automate and Chef Server.
    - Ensure to provide Chef backend's backend server IPs for Automate HA Postgres and OpenSearch machines.
    - Sample configuration; please modify according to your needs.

    ```cmd
    [architecture.existing_infra]
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    ssh_user = "myusername"
    ssh_port = "22"
    ssh_key_file = "~/.ssh/mykey.pem"
    sudo_password = ""
    # DON'T MODIFY THE BELOW LINE (backup_mount)
    backup_mount = "/mnt/automate_backups"
    # Eg.: backup_config = "object_storage" or "file_system"
    backup_config = "file_system"
    # If backup_config = "object_storage" fill out [object_storage.config] as well
    ## Object storage similar to AWS S3 Bucket
    [object_storage.config]
    bucket_name = ""
    access_key = ""
    secret_key = ""
    # For the S3 bucket, the default endpoint value is "https://s3.amazonaws.com"
    # Include protocol to the endpoint value. Eg: https://customdns1.com or http://customdns2.com
    endpoint = ""
    # [Optional] Mention object_storage region if applicable
    # Eg: region = "us-west-1"
    region = ""
    ## === ===
    [automate.config]
    # admin_password = ""
    # automate load balancer fqdn IP or path
    fqdn = "chef.example.com"
    instance_count = "2"
    # teams_port = ""
    config_file = "configs/automate.toml"
    [chef_server.config]
    instance_count = "2"
    [opensearch.config]
    instance_count = "3"
    [postgresql.config]
    instance_count = "3"
    [existing_infra.config]
    automate_private_ips = ["10.0.1.0","10.0.2.0"]
    chef_server_private_ips = ["10.0.1.0","10.0.2.0"]
    opensearch_private_ips = ["10.0.3.0","10.0.4.0","10.0.5.0"]
    postgresql_private_ips = ["10.0.3.0","10.0.4.0","10.0.5.0"]
    ```

9. Deploy using the following command:

    ```cmd
    ./chef-automate deploy config.toml --airgap-bundle <airgapped bundle name>
    ```

10. Clean up the old packages from the chef-backend (like Elasticsearch and Postgres)
11. Once done, [restore data to Chef Automate HA](/automate/ha_chef_backend_to_automate_ha/#restore-data-to-chef-automate-ha)
12. [Validate the data](/automate/ha_chef_backend_to_automate_ha/#steps-to-validate-if-migration-is-successful)

## Connect Workstation/Nodes to Automate HA

1. Open the `~/.chef/config.rb` file in your workstation and update the `chef_server_url` with the Chef-server Lb fqdn.

    Example: `chef_server_url          "https://<chef-server-lb-fqdn>/organizations/new_org"`

1. Now run `knife user list`, `knife node list`, or `knife cookbook list`. It will give you valid output.

### Updating on Nodes

1. ssh into every node and open the `/etc/chef/client.rb` file. On Windows machines, the default location for this file is `C:\chef\client.rb`. On all other systems, the default location for this file is `/etc/chef/client.rb`.
1. Update the chef-server URL with chef-server-lb fqdn.
1. Run the `chef-client` command. It will connect with the new setup, perform the scan and generate the report in chef-automate.

### Updating Nodes via Workstation

Bootstrap the nodes to update the `chef_server_url` using the following steps:

1. Open the `~/.chef/config.rb` file in your workstation and update the `chef_server_url` with the chef-server-lb fqdn.
1. Go to your workstation and open the `~/.chef/config.rb` file.
1. Update the `chef_server_url` with the chef server LB fqdn.
1. Now do node bootstrapping. It will update the chef_server_url on that node. Refer: [Node Bootstrapping](https://docs.chef.io/install_bootstrap/)

## Use Automate HA for Chef-Backend User

Download and Install the [Chef Workstation](https://www.chef.io/downloads/tools/workstation) from the Bastion machine or local machine install chef-workstation. You can refer to the [Workstation page](https://docs.chef.io/workstation/getting_started/#set-up-your-chef-repo) to set up your Workstation.

## Use Existing Private Supermarket with Automate HA

You can register a Supermarket with Automate HA in an on-premises setup. You can refer to the integration page of [Supermarket with Automate HA](/automate/supermarket_with_automate_ha/).
