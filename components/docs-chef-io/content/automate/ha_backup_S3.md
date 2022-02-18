+++
title = "Taking Backup with Amazon S3 Bucket"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Taking Backup with Amazon S3 Bucket"
    identifier = "automate/install/ha_backup_s3.md Taking Backup with Amazon S3 Bucket"
    parent = "automate/install"
    weight = 390
+++

This page explains how to take backup for External Elastic Search (ES) and Postgres-Sql to the Amazon S3 bucket.

{{< note >}}

Ensure you perform the backup configuration before deploying the Chef Automate High Availability (HA) cluster.

{{< /note >}}

## Pre-Backup Configurations

- Configure an [Amazon S3 storage bucket](https://docs.aws.amazon.com/AmazonS3/latest/userguide/create-bucket-overview.html).
- Configure [External Elasticsearch](https://docs.chef.io/automate/install/#configuring-external-elasticsearch).
- Configure AWS credentials and generate access key ID and secret access key.
- Create an IAM user in your AWS account to access the S3 bucket.
- Provide `AdministratorAccess`, `APIGatewayAdministrator` (for AWS, AmazonAPIGatewayAdministrator), `S3FullAccess` (for AWS, AmazonS3FullAccess)permissions to IAM user.
- Add the IAM role to the IAM user.
- Create an IAM policy to be associated with an IAM role. On Elasticsearch Access Policy, associate the ARN to the resource section of your bucket.
- Ensure the Chef Automate has basic [permissions](https://docs.chef.io/automate/backup/#aws-s3-permissions) to run backup operation.
- Ensure the statuses of Chef Automate services are up and running. You can check the status by typing the command, `sudo chef-automate status`.
- Create `.toml` file.

## Backup Procedure

1. Navigate to your deploy workspace. For example, `cd /hab/a2_deploy_workspace`.
2. Create directory `configs` by typing the command, `mkdir configs`.
3. Create *.toml* file by typing command, `vi /configs/automate.toml`.
4. Copy the following Ruby code into this file by altering the value of *bucket* with `bucket-name` and *name* with `bucket-name`.

``` ruby
[global.v1.external.elasticsearch.backup]

    enable = true

    location = "s3"

[global.v1.external.elasticsearch.backup.s3]

  # bucket (required): The name of the bucket

  bucket = "bucket-name"

  # base_path (optional):  The path within the bucket where backups should be stored

  # If base_path is not set, backups will be stored at the root of the bucket.

  base_path = "elasticsearch"

  # name of an s3 client configuration you create in your elasticsearch.yml

  # see https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3-client.html

  # for full documentation on how to configure client settings on your

  # Elasticsearch nodes

  client = "default"

[global.v1.external.elasticsearch.backup.s3.settings]

    ## The meaning of these settings is documented in the S3 Repository Plugin

    ## documentation. See the following links:

    ## https://www.elastic.co/guide/en/elasticsearch/plugins/current/repository-s3-repository.html



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

    # endpoint (required): The endpoint for the region the bucket lives in.

    # See https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region

    endpoint = "https://s3.amazonaws.com"

    # base_path (optional):  The path within the bucket where backups should be stored

    # If base_path is not set, backups will be stored at the root of the bucket.

    base_path = "automate"

[global.v1.backups.s3.credentials]

    # Optionally, AWS credentials may be provided. If these are not provided, IAM instance

    # credentials will be used. It's also possible for these to be read through the standard

    # AWS environment variables or through the shared AWS config files.

    # Use the credentials obtained from here [AWS-Credential](https://github.com/chef/automate-as-saas/wiki/Bastion-Setup#aws-credentials)

    access_key = "AKIARUQHMSKHGYTUJ&UI"

    secret_key = "s3kQ4Idyf9WjAgRXyv9tLYCQgYTRESDFRFV"
```

1. Save `.toml file` and exit *VI editor*.

1. Execute command, `./chef-automate patch configs/automate.toml`. This command triggers the deployment.

1. Assign the created IAM role to all the elastic search instances.

1. SSH into the automate instance by typing the command, `sudo automate-cluster-ctl ssh automate`.

1. Execute command, `./chef-automate backup create`, from the Chef Automate front-end node. The backup gets created.
