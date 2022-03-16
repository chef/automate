+++
title = "Backup and Restore"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Backup and Restore"
    identifier = "automate/install/ha_backup_restore.md Backup and Restore"
    parent = "automate/install/ha"
    weight = 70
+++

Computer security is the safeguarding of computer systems and data against theft, illegal access, or any disaster. It's the method of guarding against and detecting illegal access to your computer system. Data security refers to the process of securing data from illegal access or any disaster. This process includes the following terms:

- Data Backup
- Data Restore

## What is Data Backup?

Backup refers to the process of making copies of data or data files to use in the event the original data or data files are lost or destroyed.

In information technology, a backup or data backup is a copy of computer data taken and stored elsewhere to restore the original after a data loss event. A backup system contains at least one copy of all data considered worth saving. The data storage requirements can be large. An information repository model may be used to provide structure to this storage. Typically, backup data includes all the data such as documents, media files, configuration and registry files, and machine images.

## What is Data Restore?

Data restore is the process of copying backup data from secondary storage and restoring it to its original location or a new location. A restoring process is carried out to return lost, stolen, or damaged data to its original condition or to move data to a new location.

## What is the Difference Between Backup and Restore?

Backup and recovery are the process of duplicating data and storing it in a secure place in case of loss or damage, and then restoring that data to a location, the original one or a safe alternative that can be again used in operations to avoid downtime. Backup and recovery is also a category of onsite and cloud-based technology solutions that automate and support this process, enabling organizations to protect and retain the data for business and compliance reasons.

The key difference between backup and recovery is that the backup process is how you save and protect your production data and safely store it. Reliable backups and fast recovery together ensure business continuity and business resilience.

## What are Chef Automate High Availability (HA) Backups?

The Elastic Search, Postgres, and Chef Automate Server data and configurations can be backed up manually. There is no automated backup procedure built-in Chef Automate CLI that periodically backups the data.

## Backup Types

You can backup the Chef Automate HA data either using an External file-system (EFS) or Amazon's S3 bucket.

### What is EFS System?

External file system refers to any non-volatile storage device external to the computer. It can be any storage device that serves as an addition to the computer's primary storage, RAM, and cache memory. EFS aids in backing up the data used for future restores or disaster recovery, long-term archiving of data that is not frequently accessed, and storage of non-critical data in lower-performing, less expensive drives. These systems do not directly interact with the computer's CPU (Central Processing Unit).

External file systems include devices such as Solid-state drives (SSDs), Hard disk drives (HDDs), Cloud storage, CD-ROM drives, DVD drives, Blu-ray drives, USB flash drives, SD cards, Tape drives.

### What is Amazon's S3 Bucket?

An Amazon S3 bucket is a public cloud storage resource available in Amazon Web Services (AWS) Simple Storage Service (S3), an object storage offering. Amazon S3 buckets, similar to file folders, store objects consisting of data and its descriptive metadata. Amazon S3 is a program built to store, protect, and retrieve data from *buckets* at any time from anywhere on any device such as websites, mobile apps, archiving, data backups and restorations, IoT devices, enterprise application storage, and providing the underlying storage layer for your data lake.

With the AWS Free Usage Tier*, you can get started with Amazon S3 for free in all regions except the AWS GovCloud Regions. [See](https://aws.amazon.com/s3/) for more information.

## Taking Backup with Amazon S3 Bucket

This section explains how to take backup for External Elastic Search (ES) and Postgres-Sql to the Amazon S3 bucket.

{{< note >}}

Ensure you perform the backup configuration before deploying the Chef Automate High Availability (HA) cluster.

{{< /note >}}

### Pre-Backup Configurations

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

### Backup Procedure

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

### Restoring the S3 Backed-up Data

This section includes the procedure to restore backed-up data of the Chef Automate High Availability [HA] using EFS [External File System].

1. Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the command, `chef-automate status`.

1. Shutdown Chef Automate service on all front-end nodes by executing the command, `sudo systemctl stop chef-automate`.

1. Login to the same instance of Chef Automate front-end node from which backup is taken.

1. Execute the restore command, `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"`.

1. Start all Chef Automate and Chef Infra Server front-end nodes by executing the command, `sudo systemctl start chef-automate`.

## Backup with EFS System

This section explains how to take backup and restore for External Elastic Search (ES) and Postgres-Sql on External File-System (EFS). You can take the backup and restore on EFS system through DNS or IP.

A shared file system requires you to create Elasticsearch snapshots. To mount the same shared filesystem to the same location on all master and data nodes, register these snapshot repositories with Elasticsearch.

You must register this location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

{{< note >}}

Ensure you perform the backup configuration before deploying the Chef Automate High Availability (HA) cluster.

{{< /note >}}

### Pre-Backup Configurations

- Create the EFS over the AWS.
- Open the port `2049` Proto(NFS) for the EFS security group.

### Backup Procedure

Let us assume that the shared filesystem is mounted to `/mnt/automate_backups`. Now, follow these steps to configure the Chef Automate High Availability (HA) to register the snapshot locations with Elasticsearch:

1. Enter the `mount /mnt/automate_backups` command to ensure the shared file system exists on all Elasticsearch servers.

1. Create Elasticsearch sub-directory and set permissions by executing the following commands:

```bash
sudo mkdir /mnt/automate_backups/elasticsearch
sudo chown hab:hab /mnt/automate_backups/elasticsearch/
```

{{< note >}}

If the network is mounted correctly, you need to perform this step on a single Elasticsearch server.

{{< /note >}}

1. Export the current Elasticsearch configuration from the Habitat supervisor.

1. Log in as a root user.

1. SSH to a single Elasticsearch server and configure Elasticsearch `path.repo` setting by executing the following commands:

```bash
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-backend-elasticsearch | tail -n +2 > es_config.toml
```

1. Edit `es_config.toml` to add the following settings at the end of the file:

```ruby
[es_yaml.path]
   # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb
   repo = "/mnt/automate_backups/elasticsearch"
```

{{< note >}}

This file may be empty if credentials are never rotated.

{{< /note >}}

1. Apply updated `es_config.toml` configuration to Elasticsearch by executing the following commands:

```bash
hab config apply automate-backend-elasticsearch.default $(date '+%s') es_config.toml
hab svc status (check elasticsearch service is up or not)
curl -k -X GET "https://localhost:9200/_cat/indices/*?v=true&s=index&pretty" -u admin:admin
   # Watch for a message about Elasticsearch going from RED to GREEN
`journalctl -u hab-sup -f | grep 'automate-ha-elasticsearch'
```

You can perform this application only once, which triggers a restart of the Elasticsearch services on each server.

1. Configure Chef Automate HA to handle external Elasticsearch backups by adding the following configuration to `/hab/a2_deploy_workspace/config/automate.toml` on the provisioning host or from the bastion host:

```ruby
[global.v1.external.elasticsearch.backup]
   enable = true
   location = "fs"

   [global.v1.external.elasticsearch.backup.fs]
   # The `path.repo` setting you've configured on your Elasticsearch nodes must be
   # a parent directory of the setting you configure here:
   path = "/mnt/automate_backups/elasticsearch"

   [global.v1.backups.filesystem]
   path = "/mnt/automate_backups/backups"
```

1. Enter the `./chef-automate config patch automate.toml` command to apply the patch configuration to the Chef Automate HA servers. This command also triggers the deployment.

1. Enter the `chef-automate backup create` command from a Chef Automate front-end node to create a backup.

### Restoring the EFS Backed-up Data

This section includes the procedure to restore backed-up data of the Chef Automate High Availability [HA] using AWS [Amazon Web Services] S3 bucket.

1. Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the command, `chef-automate status`.

1. Shutdown Chef Automate service on all front-end nodes by executing the command, `sudo systemctl stop chef-automate`.

1. Login to the same instance of Chef Automate front-end node from which backup is taken.

1. Execute the restore command, `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

1. Start all Chef Automate and Chef Infra Server front-end nodes by executing the command, `sudo systemctl start chef-automate`.
