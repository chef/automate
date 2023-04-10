+++
title = "Automate to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Automate to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_automate_to_automate_ha.md Automate to Automate HA"
    weight = 220
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}

- Minimum version of Automate from which migration can be performed is 4.x

- Customers in 2022xxxx or 3.x version needs to update to 4.x version.

- Automate HA with AWS managed needs to use OpenSearch version <1.3.2

- The OpenSearch version in Automate should be of same version as of Automate HA.

{{< /warning >}}

## Upgrade with FileSystem Backup Locally

{{< warning >}}

- The below steps won't work for Automate HA with AWS Managed.

{{< /warning >}}

1. Create Backup of Chef Automate Standalone using the following command:

    ```bash
    chef-automate backup create
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will create the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml` file.
    - The second command will create the `bootstrap.abb` this bundle captures any local credentials or secrets that aren’t persisted in the database.
    - Once the backup is completed, save the backup Id. For example: `20210622065515`.

1. Go to `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml` file and create **Bundle** using the following command:

    ```bash
    tar -cvf backup.tar.gz <backup_id>/ automatebackup-elasticsearch/ .tmp/
    ```

1. Transfer the `tar` bundle to one of the Chef Automate HA Frontend Nodes.

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server).

1. Go to Bastion 
    
    - Create a .toml (say os_config.toml) file in the Bastion host, copy the following contents and then patch this file in all the OpenSearch nodes. 
    ```bash
    [path] 
      repo = "/mnt/automate_backups" 
    ```
    
    Following command will patch the configuration in all the OpenSearch nodes. 
    `chef-automate config patch --opensearch <PATH TO OS_CONFIG.TOML>`

    - Create a .toml (say automate.toml) file in the Bastion host, copy the following content and then patch this file in all the Frontend nodes.
    ```bash
    [global.v1.external.opensearch.backup]
        enable = true 
        location = "fs" 
    [global.v1.external.opensearch.backup.fs] 
        path = "/mnt/automate_backups" 
    [global.v1.backups.filesystem]
        path = "/mnt/automate_backups" 
    ```

    Following command will patch the configuration in all the Frontend nodes: 
    `chef-automate config patch --fe <PATH TO automate.toml>`

1. Go the Chef Automate HA, where we copied the `tar` file. Unzip the bundle using:

    ```bash
    tar -xf backup.tar.gz -C /mnt/automate_backups
    ```

1. Run the following command at Chef-Automate node of Automate HA cluster to get the applied `config`:

    ```bash
    sudo chef-automate config show > current_config.toml 
    ```

    From Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the config. Add the OpenSearch password to the generated config above. For example:

    ```bash
    [global.v1.external.opensearch.auth.basic_auth]
    username = "admin"
    password = "admin"
    ```

    {{< warning >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

1. Stop all the services at frontend nodes in Automate HA Cluster. Run the following command to all the Automate and Chef Infra Server nodes:

    ``` bash
    sudo chef-automate stop
    ```

1. Unpack the `bootstrap.abb` file on all the Frontend nodes:

    Login to Each Frontend Node and then run after copying the `bootstrap.abb` file.

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Restore in Chef-Automate HA using the following command:

    ```bash
    automate_version_number=4.x.y ## Please change this to the version of Chef Automate HA installed. Look for /var/tmp/frontend-4.x.y.aib file
     
    chef-automate backup restore /mnt/automate_backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ```

1. Start the Service in all the Frontend Nodes with the command shown below:

    ```bash
    sudo chef-automate start
    ```

## Upgrade with FileSystem Backup via Volume Mount

1. Create *Backup* of Chef Automate Standalone using the following command:

    ```bash
    chef-automate backup create
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will create the backup at the file mount location mentioned in the `config.toml` file.
    - The second command will create the `bootstrap.abb`.
    - Once the backup is completed, save the backup Id. For example: `20210622065515`

1. Detach the File system from Standalone Chef-Automate.

1. Attach and Mount the same file system to the Automate-HA all the nodes:

    - Make sure that it should have permission for hab user

1. Stop all the services at frontend nodes in Automate HA Cluster. Run the below command to all the Automate and Chef Infra Server nodes

    ``` bash
    sudo chef-automate stop
    ```

1. Get the Automate HA version number from the location `/var/tmp/` in Automate instance. For example : `frontend-4.x.y.aib`

1. Run the command at Chef-Automate node of Automate HA cluster to get the applied config:

    ```bash
    sudo chef-automate config show > current_config.toml 
    ```

    From Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the config. Add the OpenSearch password to the generated config above. For example:

    ```bash
    [global.v1.external.opensearch.auth.basic_auth]
    username = "admin"
    password = "admin"
    ```

    {{< warning >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

1. Copy the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server).

1. Unpack the `bootstrap.abb` file on all the Frontend nodes. `ssh` to Each Frontend Node and run the following command:

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Run the restore command in one of the Chef Automate node in Chef-Automate HA cluster:

    ```bash
    automate_version_number=4.x.y ## Please change this to the version of Chef Automate HA installed. Look for /var/tmp/frontend-4.x.y.aib file

    chef-automate backup restore /mnt/automate_backups/backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ```

1. Start the Service in All the Frontend Nodes with command shown below:

    ``` bash
    sudo chef-automate start
    ```
