+++
title = "Backup and restore using Object Storage | On-Permise Deployment"

draft = false

gh_repo = "automate"

[menu]
[menu.automate]
title = "Prerequisites"
identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore Object Storage - On-Premise"
parent = "automate/deploy_high_availability/backup_and_restore"
weight = 210
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< note >}}

-   This page explains the configuration for backup with **object storage** and Restore with On-Premise deployment procedure.
-   If user choose `backup_config` as `object_storage` in `config.toml,` backup is already configured during the deployment, and in that case **the below steps are not required and can be skipped**. i.e., **`backup_config = "object_storage"`** . If `backup_config` left blank, then configuration needs to be done for backup and restore.

{{< /note >}}

## Overview

This section provides the pre-backup configuration required to backup the data on Object Storage System (Other than AWS S3) like Minio, Non-AWS S3. The steps to set a secret key using commands are given below:

#### Configuration in Opensearch Node

This section provides the pre-backup configuration required to backup the data on Object Storage System (Similar to but not AWS S3) like _Minio_, _Non-AWS S3_. The steps to set a secret key using commands are given below:

1. Log in to all the opensearch nodes and follow the steps on all the opensearch nodes.

-   Export `OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"`
-   `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.access_key` (When asked, Enter your key)
-   `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.secret_key` (When asked, Enter your key/secret)
-   `chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore` (Setting hab:hab permission)
-   `curl -k -X POST "https://127.0.0.1:9200/_nodes/reload_secure_settings?pretty" -u admin:admin` (Command to load the above setting)

The final output after running the command 1.5 on the third node is given below:

```json
{
	"_nodes": {
		"total": 3,
		"successful": 3,
		"failed": 0
	},
	"cluster_name": "chef-insights",
	"nodes": {
		"lenRTrZ1QS2uv_vJIwL-kQ": {
			"name": "lenRTrZ"
		},
		"Us5iBo4_RoaeojySjWpr9A": {
			"name": "Us5iBo4"
		},
		"qtz7KseqSlGm2lEm0BiUEg": {
			"name": "qtz7Kse"
		}
	}
}
```

2. To override the existing default endpoint:

-   Login to one of the open search instances and run the following command (You need root access to run the command):

```sh
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-ha-opensearch | tail -n +2 > es_config.toml
```

-   Edit the created `es_config.toml` file and add the following settings at the end of the file. (_The file will be empty if the credentials have not been rotated_)

```sh
[s3]
  [s3.client.default]
    protocol = "https"
    read_timeout = "60s"
    max_retries = "3"
    use_throttle_retries = true
    # Add endpoint of the Object storage below
    endpoint = ""
```

-   Run the following command to apply the updated `es_config.toml` changes. Run this command only once. (_This will trigger a restart of the OpenSearch services on each server_)

```sh
hab config apply automate-ha-opensearch.default $(date '+%s') es_config.toml
```

-   Once done with the above steps, run the following command:

```sh
journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
```

The screen will display a message of OpenSearch going from **RED/YELLOW** to **GREEN**.

#### Healthcheck commands

To be filled

#### Configuration in Provision host

{{< note >}}

Make sure all the frontend nodes and opensearch have access to the object storage. (IAM role)

{{< /note >}}

Once done with the opensearch setup, add the following `automate.toml` file and patch the `config`. In the file, modify the values listed below:

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
./chef-automate config patch /path/to/automate.toml
```

## Backup and Restore

### Backup

Chef Automate let's you create a new backup. You can create it by running the backup command from a Chef Automate front-end node (chef-server or automate node). The backup command is as shown below:

```cmd
chef-automate backup create
```

<!-- ### Restore

This section includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using File System.

Restore operation restores all the data while the backup is going on. The restore operation stops will the ongoing backup procedure. Let's understand the whole process by a scenario:

-   Create a automate _UserA_ and generate an API token named _Token1_ for _UserA_.
-   Create a backup, and let's assume the back id to be _20220708044530_.
-   Create a new user _UserB_ and a respective API token named _Token2_.
-   Now, suppose you want to restore data in the same automate cluster. In that case, the data will only be stored for _UserA_ with its token as the backup bundle only contains the _UserA_, and the _UserB_ is not available in the backup bundle. -->

#### Restoring the Backed-up Data from Object Storage

To restore backed-up data of the Chef Automate High Availability (HA) using External Object Storage, follow the steps given below:

-   Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

-   Shutdown Chef Automate service on all front-end nodes

    -   Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
    -   Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server

-   Log in to the same instance of Chef Automate front-end node from which backup is taken.

-   Execute the restore command `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key"`.

{{< figure src="/images/automate/ha_restore.png" alt="Restore">}}

-   Ideally all Chef Automate and Chef Infra Server front-end nodes should start by end of restoring. If not, start them by executing the `sudo systemctl start chef-automate` command.

{{< figure src="/images/automate/ha_restore_success.png" alt="Restore Success">}}
