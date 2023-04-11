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

The below steps won't work for Automate HA with AWS Managed.

{{< /warning >}}

1. Create Backup of Chef Automate Standalone using the following command:

    ```bash
    chef-automate backup create
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will create the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml` file.
    - The second command will create the `bootstrap.abb` bundle, this bundle captures any local credentials or secrets that aren’t persisted in the database.
    - Once the backup is completed, save the backup Id. For example: `20210622065515`.

1. Go to `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml` file and create **Bundle** using the following command:

    ```bash
    tar -cvf backup.tar.gz <backup_id>/ automatebackup-elasticsearch/ .tmp/
    ```

1. Transfer the `tar` bundle to one of the Chef Automate HA Frontend Nodes using the following command:
    `scp -i <path to your .pem file> <path to backup.tar.gz> ec2-user@<IP>:/home/ec2-user`

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:
    `scp -i <path to your .pem file> <path to bootstrap.abb> ec2-user@<IP>:/home/ec2-user`

1. Go to Bastion 
    - Create a .toml (say os_config.toml) file in the Bastion host, copy the following contents and then patch this file in all the OpenSearch nodes.
    
    ```bash
    [path] 
      repo = "/mnt/automate_backups" 
    ```
    
    Following command will patch the configuration in all the OpenSearch nodes. 
    `chef-automate config patch --opensearch <path to os_config.toml>`
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
    `chef-automate config patch --fe <Path to automate.toml>`

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

{{< warning >}}

The below steps won't work for Automate HA with AWS Managed.

{{< /warning >}}

1. Make EFS volume and attach that volume to existing automate setup and Automate HA nodes. 

1. Mount EFS Volume:
    - In automate we are mounting that EFS volume at `/var/opt/chef-automate/backups` location unless you specify the location in `config.toml` file. 

    - In HA we are mounting that volume at `/mnt/automate_backups`. (You need to mount this volume in all the HA nodes).
    {{< warning >}}
    Make sure that it should have permission for hab user.
    {{< /warning >}}

1. Go to Bastion 
    - Create a .toml (say os_config.toml) file in the Bastion host, copy the following contents and then patch this file in all the OpenSearch nodes.
    
    ```bash
    [path] 
      repo = "/mnt/automate_backups" 
    ```
    
    Following command will patch the configuration in all the OpenSearch nodes. 
    `chef-automate config patch --opensearch <Path to os_config.toml>`
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
    `chef-automate config patch --fe <Path to automate.toml>`

1. Create *Backup* of Chef Automate Standalone using the following command:

    ```bash
    chef-automate backup create
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will create the backup at the file mount location mentioned in the `config.toml` file.
    - The second command will create the `bootstrap.abb` bundle, this bundle captures any local credentials or secrets that aren’t persisted in the database.
    - Once the backup is completed, save the backup Id. For example: `20210622065515`.

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

1. Stop all the services at frontend nodes in Automate HA Cluster. Run the below command to all the Automate and Chef Infra Server nodes

    ``` bash
    sudo chef-automate stop
    ```

1. Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:
    `scp -i <path to your .pem file> <path to bootstrap.abb> ec2-user@<IP>:/home/ec2-user`

1. Unpack the `bootstrap.abb` file on all the Frontend nodes:

    Login to Each Frontend Node and then run after copying the `bootstrap.abb` file.

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
    ```

1. Run the restore command in one of the Chef Automate node in Chef-Automate HA cluster:

    ```bash
    automate_version_number=4.x.y ## Please change this to the version of Chef Automate HA installed. Look for /var/tmp/frontend-4.x.y.aib file

    chef-automate backup restore /mnt/automate_backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ```

1. Start the Service in All the Frontend Nodes with command shown below:

    ``` bash
    sudo chef-automate start
    ```

## Upgrade with S3

1. Patch the following configuration in Standalone Chef Automate for creating the backup in S3 bucket. 

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
    # Optionally, AWS credentials may be provided. If these are not provided, IAM instance 
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

1. Create Backup of Chef Automate Standalone using the following command:

    ```bash
    chef-automate backup create
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will create the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml` file.
    - The second command will create the `bootstrap.abb` bundle, this bundle captures any local credentials or secrets that aren’t persisted in the database.
    - Once the backup is completed, save the backup Id. For example: `20210622065515`.

1.  Transfer the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server) using the following command:

    ```bash
    scp -i <path to your .pem file> <path to bootstrap.abb> ec2-user@<IP>:/home/ec2-user
    ```

1. Go to Bastion:
    {{< warning >}}
    - Use the same bucket which you used in standalone automate while creating the backup.
    - Configure the same basepath in Automate HA, that you have given in Standalone Automate. 
    For example: In Standalone Automate, while patching the configurations of S3 bucket we haven’t provided anything in base_path field. 
    {{< /warning >}}

    - Create a .toml (say automate.toml) file in the Bastion host, copy the following content and then patch this file in all the Frontend nodes:

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
            secret_key = "<Your Seecret Key>"
    ```

    Following command will patch the configuration in all the Frontend nodes: 
    `chef-automate config patch --frontend automate.toml`

1. Unpack the `bootstrap.abb` file on all the Frontend nodes:

    Login to Each Frontend Node and then run after copying the `bootstrap.abb` file.

    ```bash
    chef-automate bootstrap bundle unpack bootstrap.abb
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

1. Stop all the services at frontend nodes in Automate HA Cluster. Run the below command to all the Automate and Chef Infra Server nodes

    ``` bash
    sudo chef-automate stop
    ```

1. Run the restore command in one of the Chef Automate node in Chef-Automate HA cluster:

    ```bash
    automate_version_number=4.x.y ## Please change this to the version of Chef Automate HA installed. Look for /var/tmp/frontend-4.x.y.aib file

    sudo chef-automate backup restore s3://<s3-bucket-name>/<path-to-backup>/<backup-id>/ --patch-config /path/to/current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
    ```

1. Start the Service in All the Frontend Nodes with command shown below:

    ``` bash
    sudo chef-automate start
    ```