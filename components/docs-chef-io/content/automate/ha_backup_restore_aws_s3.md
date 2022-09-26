+++
title = "Prerequisites"

draft = false

gh_repo = "automate"

[menu]
[menu.automate]
title = "Prerequisites"
identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore Prerequisites"
parent = "automate/deploy_high_availability/backup_and_restore"
weight = 210
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< note >}}

-   This page explains the prerequisites of the backup with AWS deployment procedure.
-   If user choose `backup_config` as `s3` in `config.toml,` backup is already configured during deployment, **the below steps are not required and can be skipped**. i.e., **`backup_config = "efs"`** . If we have kept the `backup_config` blank, we need to perform the below steps.

{{< /note >}}

### Overview

### Pre Backup Configuration for S3 Backup

To run the terraform scripts, the IAM users should have proper permissions. Going forward, we will also discuss the required permissions. You should have your `secret access key` and `key id`, or else you can regenerate a new access key.

**Permissions Required**

Check if the IAM user has all the required permissions. The permission policies are listed below:

1. AdministratorAccess

1. APIGatewayAdministrator (For aws AmazonAPIGatewayAdministrator)

1. S3FullAccess (for aws AmazonS3FullAccess)

Create an IAM role to give access of **S3** to **OpenSearch** instances. The role should already be assigned as the OpenSearch instance tries to communicate S3.

The permissions can either be directly added to the user or added via **IAM Group**.

Once done with the above steps, `.toml` file and patch the `.config`. In the file, modify the values listed below:

1. bucket name -

    - `bucket = "bucket-name"`
    - `name = "bucket-name"`

2. `mkdir configs`

3. `vi configs/automate.toml`

Refer to the content for the `automate.toml` file below:

```sh
[global.v1]
  [global.v1.external.opensearch.backup]
    enable = true
    location = "s3"

  [global.v1.external.opensearch.backup.s3]

    # bucket (required): The name of the bucket
    bucket = "bucket-name"

    # base_path (optional):  The path within the bucket where backups should be stored
    # If base_path is not set, backups will be stored at the root of the bucket.
    base_path = "opensearch"

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
    base_path = "automate"

  [global.v1.backups.s3.credentials]
    access_key = "AKIAO"
    secret_key = "s3kQ"
```

Execute the command given below to trigger the deployment.

```sh
./chef-automate config patch configs/automate.toml
```

Take a [back-up](/automate/ha_restore/) of the configurations once the cluster has been deployed.

{{< note >}} **IAM Role:** Assign the IAM Role to all the OpenSearch instances in the cluster created above. {{< /note >}}
