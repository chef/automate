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

- Standalone Chef Automate or Chef Automate with embedded Chef Infra Server can migrate to Automate HA, with 
minimum version of Chef Automate: [2020xxxxxxxx](https://docs.chef.io/release_notes_automate/)

{{< /warning >}}

## Migration with FileSystem Backup Locally

Follow the below steps, when you are migrating to On-Premises or AWS HA deployment (but not for AWS with managed services).

1. Create a Backup of Chef Automate Standalone using the following command:

    1. Run the below command to create the backup in the `/var/opt/chef-automate/backups` location unless you specify the location in the `config.toml` file.

    ```bash
    chef-automate backup create
    ```

    Once the backup is completed, save the backup Id. For example: `20210622065515`.

    1. Run the below command to create the `bootstrap.abb` bundle. This bundle captures any local credentials or secrets not persisted in the database.

    ```bash
    chef-automate bootstrap bundle create bootstrap.abb
    ```


1. If you haven't specified the location in the `config.toml` file, go to the `/var/opt/chef-automate/backups` location and create **Bundle** using the following command:

    ```bash
    tar -cvf backup.tar.gz <backup_id>/ automatebackup-elasticsearch/ .tmp/
    ```

1. Transfer the `tar` bundle to one of the Chef-Automate node of Automate HA using the following command:

    ```bash
        scp -i /path/to/key /path/to/backup-file user@host:/home/user
    ```

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:

    ```bash
        scp -i /path/to/key /path/to/bootstrap.abb user@host:/home/user
    ```

1. Go to Bastion and:
    - Create a `.toml` (say os_config.toml) file in the Bastion host. Once done, copy the following contents to the `.toml` file and patch the file in all the OpenSearch nodes.

    ```bash
    [path]
      repo = "/mnt/automate_backups"
    ```

    The following command will patch the configuration in all the OpenSearch nodes.

    ```bash
    chef-automate config patch --opensearch <path to os_config.toml>
    ```

    - Create a `.toml` (say automate.toml) file in the Bastion host. Once done, copy the following content to the `.toml` file and then patch the file in all the Frontend nodes.

    ```bash
    [global.v1.external.opensearch.backup]
        enable = true
        location = "fs"
    [global.v1.external.opensearch.backup.fs]
        path = "/mnt/automate_backups"
    [global.v1.backups.filesystem]
        path = "/mnt/automate_backups"
    ```

    The following command will patch the configuration in all the Frontend nodes:

    ```bash
    chef-automate config patch --fe <Path to automate.toml>
    ```

1. Go to the Chef-Automate node of Automate HA cluster, where we copied the `tar` file. Unzip the bundle using the following:

    ```bash
    tar -xf backup.tar.gz -C /mnt/automate_backups
    ```

1. Run the following command at the Chef-Automate node of Automate HA cluster to get the applied `config`:

    ```bash
    sudo chef-automate config show > current_config.toml
    ```

    From Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the `config`. Add the OpenSearch password to the generated config above. For example:

    ```bash
    [global.v1.external.opensearch.auth.basic_auth]
    username = "admin"
    password = "admin"
    ```

    {{< warning >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

1. On every Frontend Node copy the `bootstrap.abb` file and unpack the file using the following command:

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Stop all the instances except where you saved the `.tar` file at frontend nodes in Automate HA Cluster. Run the following command to all the Automate and Chef Infra Server nodes:

    ``` bash
    sudo chef-automate stop
    ```

1. Restore in Chef-Automate HA using the following command:

    ```bash
    chef-automate backup restore /mnt/automate_backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ## Refer to the `/var/tmp/frontend-4.x.y.aib` file name for the exact version number.
    ```

1. Start the Service in all the Frontend Nodes using the following command:

    ```bash
    sudo chef-automate start
    ```

## Migration with FileSystem Backup via Volume Mount

Follow the below steps, when you are migrating to On-Premises or AWS HA deployment (but not for AWS with managed services).

1. Make EFS volume and attach that volume to the existing automate and Automate HA nodes.
1. Mount EFS Volume:
    - In Automate, we are mounting that EFS volume at the `/var/opt/chef-automate/backups` location unless you specify the location in the `config.toml` file.
    - In HA, we are mounting that EFS volume at `/mnt/automate_backups`. (You need to mount this volume in all the HA nodes).

    Make sure that the location have permission for the hab user.

1. Go to Bastion and:
    - Create a `.toml` (say os_config.toml) file in the Bastion host. Once done, copy the following contents to the `.toml` file and patch the file in all the OpenSearch nodes.

    ```bash
    [path]
      repo = "/mnt/automate_backups"
    ```

    The following command will patch the configuration in all the OpenSearch nodes.

    ```bash
    chef-automate config patch --opensearch <Path to os_config.toml>
    ```

    - Create a `.toml` (say automate.toml) file in the Bastion host. Once done, copy the following content to the `.toml` file and then patch the file in all the Frontend nodes.

    ```bash
    [global.v1.external.opensearch.backup]
        enable = true
        location = "fs"
    [global.v1.external.opensearch.backup.fs]
        path = "/mnt/automate_backups"
    [global.v1.backups.filesystem]
        path = "/mnt/automate_backups"
    ```

    The following command will patch the configuration in all the Frontend nodes:

    ```bash
    chef-automate config patch --fe <Path to automate.toml>
    ```

1. Create a Backup of Chef Automate Standalone using the following command: 

    1. Run the below command to create the backup in the `/var/opt/chef-automate/backups` location unless you specify the location in the `config.toml` file.

    ```bash
    chef-automate backup create
    ```

    Once the backup is completed, save the backup Id. For example: `20210622065515`.

    1. Run the below command command to create the `bootstrap.abb` bundle. This bundle captures any local credentials or secrets not persisted in the database.

    ```bash
    chef-automate bootstrap bundle create bootstrap.abb
    ```

1. Run the following command at the Chef-Automate node of Automate HA cluster to get the applied config:

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

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:

    ```bash
        scp -i /path/to/key /path/to/bootstrap.abb user@host:/home/user
    ```

1. Unpack the `bootstrap.abb` file on all the Frontend nodes:

    Login to Each Frontend Node and then run after copying the `bootstrap.abb` file.

    ```bash
        chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Stop all the frontend instances except where you saved the `current_config.toml` file at automate nodes in Automate HA Cluster. Run the following command to all the Automate and Chef Infra Server nodes:

    ``` bash
    sudo chef-automate stop
    ```

1. Restore in Chef-Automate HA using the following command:

    ```bash
    chef-automate backup restore /mnt/automate_backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ## Refer to the `/var/tmp/frontend-4.x.y.aib` file name for the exact version number.
    ```

1. Start the Service in all the Frontend Nodes using the following command:

    ```bash
    sudo chef-automate start
    ```

## Migration with S3

For AWS managed services, map the snapshot role to OpenSearch dashboard. It is necessary to [enable backup and restore in OpenSearch](automate/managed_services/#enabling-opensearch-backup-restore).

1. Patch the following configuration in Standalone Chef Automate for creating the backup in the S3.

    ```bash
    [global.v1.backups]
    location = "s3"
    [global.v1.backups.s3.bucket]
    # name (required): The name of the bucket
    name = "<bucket name>"
    # endpoint (required): The endpoint for the region the bucket lives in for Automate Version 3.x.y
    # endpoint (required): For Automate Version 4.x.y, use this https://s3.amazonaws.com
    endpoint = "https://s3.amazonaws.com"
    # base_path (optional):  The path within the bucket where backups should be stored
    # If base_path is not set, backups will be stored at the root of the bucket.
    base_path = ""
    [global.v1.backups.s3.credentials]
    # Optionally, AWS credentials may be provided. If these are not provided, the IAM instance
    # credentials will be used. It's also possible for these to be read through the standard
    # AWS environment variables or through the shared AWS config files.
    access_key = "<access_key>"
    secret_key = "<secret_key>"
    session_key = "<session_key>"
    [global.v1.backups.s3.ssl]
    # root_cert (optional): The root certificate used for SSL validation.
    # For S3 compatible APIs, you can set the SSL root cert if needed
    root_cert = """
    -----BEGIN CERTIFICATE-----
    ...
    -----END CERTIFICATE-----
    """
    ```

1. Create a Backup of Chef Automate Standalone using the following command:

    1. Run the below command to create the backup in the `/var/opt/chef-automate/backups` location unless you specify the location in the `config.toml` file.

    ```bash
    chef-automate backup create
    ```

    Once the backup is completed, save the backup Id. For example: `20210622065515`.

    1. Run the below command command to create the `bootstrap.abb` bundle. This bundle captures any local credentials or secrets not persisted in the database.

    ```bash
    chef-automate bootstrap bundle create bootstrap.abb
    ```

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:

    ```bash
    scp -i /path/to/key /path/to/bootstrap.abb user@host:/home/user
    ```

1. Go to Bastion:

    {{< note >}} Use the same bucket for restore which was used in the standalone automate while creating the backup. Configure the same basepath in Automate HA you gave in Standalone Automate. {{< /note >}}

    - Create a `.toml` (say os_config.toml) file in the Bastion host. Once done, copy the following contents to the `.toml` file and patch the file in all the OpenSearch nodes.

    ```bash
    [global.v1]
        [global.v1.external.opensearch.backup]
            enable = true
            location = "s3"
        [global.v1.external.opensearch.backup.s3]
            # bucket (required): The name of the bucket
            bucket = "bucket-name"
            # base_path (optional):  The path within the bucket where backups should be stored
            # If base_path is not set, backups will be stored at the root of the bucket.
            base_path = ""
            # name of an s3 client configuration you create in your opensearch.yml
            # see https://www.open.co/guide/en/opensearch/plugins/current/repository-s3-client.html
            # for full documentation on how to configure client settings on your
            # OpenSearch nodes
            client = "default"
        [global.v1.external.opensearch.backup.s3.settings]
            ## The meaning of these settings is documented in the S3 Repository Plugin
            ## documentation. See the following links:
            ## https://www.open.co/guide/en/opensearch/plugins/current/repository-s3-repository.html
            ## Backup repo settings
            # compress = false
            # server_side_encryption = false
            # buffer_size = "100mb"
            # canned_acl = "private"
            # storage_class = "standard"
            ## Snapshot settings
            # max_snapshot_bytes_per_sec = "40mb"
            # max_restore_bytes_per_sec = "40mb"
            # chunk_size = "null"
            ## S3 client settings
            # read_timeout = "50s"
            # max_retries = 3
            # use_throttle_retries = true
            # protocol = "https"
        [global.v1.backups]
            location = "s3"
        [global.v1.backups.s3.bucket]
            # name (required): The name of the bucket
            name = "bucket-name"
            # endpoint (required): The endpoint for the region the bucket lives in for Automate Version 3.x.y
            # endpoint (required): For Automate Version 4.x.y, use this https://s3.amazonaws.com
            endpoint = "https://s3.amazonaws.com"
            # base_path (optional):  The path within the bucket where backups should be stored
            # If base_path is not set, backups will be stored at the root of the bucket.
            base_path = ""
        [global.v1.backups.s3.credentials]
            access_key = "<Your Access Key>"
            secret_key = "<Your Secret Key>"
    ```

    The following command will patch the configuration in all the Frontend nodes:

    ```bash
    chef-automate config patch --frontend automate.toml
    ```

1. Run the following command at the Chef-Automate node of Automate HA cluster to get the applied `config`:

    ```bash
    sudo chef-automate config show > current_config.toml
    ```

    From Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the `config`. Add the OpenSearch password to the generated config above. For example:

    ```bash
    [global.v1.external.opensearch.auth.basic_auth]
    username = "admin"
    password = "admin"
    ```

    {{< warning >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

1. On every Frontend Node copy the `bootstrap.abb` file and unpack the file using the following command:

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Stop all the frontend instances except where you saved the `current_config.toml` file at automate node in Automate HA Cluster. Run the following command to all the Automate and Chef Infra Server nodes:

    ``` bash
    sudo chef-automate stop
    ```

1. Restore in Chef-Automate HA using the following command:

    ```bash
    sudo chef-automate backup restore s3://<s3-bucket-name>/<path-to-backup>/<backup-id>/ --patch-config /path/to/current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
    ## Refer to the `/var/tmp/frontend-4.x.y.aib` file name for the exact version number.
    ```

1. Start the Service in All the Frontend Nodes using the following command:

    ```bash
    sudo chef-automate start
    ```

## Steps to Validate migration is successful

1. Check the Automate UI of Automate HA. Check whether the data is present in Automate UI for HA.
1. If you are using the embedded chef server, log in to the Chef-server HA node, and run the following commands:
    - `knife user list`: will give the user list you created in standalone automate.
    - `knife opc org list`: It will give the organization list you created in standalone automate.

You can also [validate the migration](/automate/ha_chef_backend_to_automate_ha/#steps-to-validate-if-migration-is-successful) using one of the other ways. You can check out the steps to  connect to the new setup.

## Troubleshoot

1. While running the restore command, If you are getting this error: `Failed to restore synchronous operations`, follow the steps given below:

    1. Log in to any Automate Node in HA.
    1. Run the below command to get all the snapshots.

        ```bash
        curl -k -X GET -s http://localhost:10144/_snapshot/_all?pretty
        ```

    1. One by One, delete all the snapshots using the below command.

        ```bash
        curl -k -X DELETE -s http://localhost:10144/_snapshot/<snapshot_name>
        ```

        Example:

        ```bash
        curl -k -X DELETE -s http://localhost:10144/_snapshot/chef-automate-es6-event-feed-service 
        ```

1. While running the restore command, If you are getting this error: `Path is not accessible on master node`, follow the steps given below:
    1. Log in to any Automate HA Opensearch Node, and run the below command:

        ```bash
        chmod 777 -R /mnt/automate_backups/
        ```

1. If you are getting `The hab user doesn't have read/write/exec permission on the backup repository` error, run the below command:

    ```bash
    sudo chown hab:hab /mnt/automate_backups
    ```
