+++
title = "On-Premise Deployment using Object Storage"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-Premise Deployment using Object Storage"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore Object Storage"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 220
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< note >}}

- If the user chooses `backup_config` as `object_storage` in `config.toml` backup is already configured during the deployment, and in that case **the below steps are not required**. If `backup_config` is left blank, then the configuration needs to be configured manually.

{{< /note >}}

## Overview

This section provides the pre-backup configuration required to back up the data on Object Storage System (Other than AWS S3) like Minio, Non-AWS S3. The steps to set a secret key using commands are given below:

### Configuration in OpenSearch Node

This section provides the pre-backup configuration required to back up the data on Object Storage Systems like _Minio_, _Non-AWS S3_. The steps to set a secret key using commands are given below:

1. Log in to all the OpenSearch nodes and follow the steps on all the OpenSearch nodes.

- `export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"`
- `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.access_key` (When asked, Enter your key)
- `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.secret_key` (When asked, Enter your key/secret)
- `chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore` (Setting hab:hab permission)
- `curl -k -X POST "https://127.0.0.1:9200/_nodes/reload_secure_settings?pretty" -u admin:admin` (Command to load the above setting)

The final output after running the curl command on all nodes is given below:

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

#### Configuration for Opensearch Node from Provision Host

1. To override the existing default endpoint:

- Create an `.toml` file on **the provisioning server** using the following command:

    ```bash
    touch os_config.toml
    ```

- Add the following settings at the end of the `os_config.toml` file.

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

- Run the following command to apply the updated `os_config.toml` changes. Run this command only once. (_This will trigger a restart of the OpenSearch services on each server_)

    ```sh
    chef-automate config patch --opensearch os_config.toml
    ```

This will update the configuration in Opensearch node.

#### Healthcheck commands

- Following command can be run in the OpenSearch node

    ```sh
    hab svc status (check whether OpenSearch service is up or not)

    curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check is to check whether all the indices are green or not)

    # Watch for a message about OpenSearch going from RED to GREEN
    `journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```

#### Configuration for Automate Node from Provision Host

{{< note >}}

Make sure all the frontend nodes and OpenSearch have access to the object storage.

{{< /note >}}

Once done with the OpenSearch setup, add the following `automate.toml` file and patch the updated config to all frontend nodes. In the file, modify the values listed below:

1. Create .toml file by `vi automate.toml`

2. Refer to the content for the `automate.toml` file below:

    ```sh
    [global.v1]
      [global.v1.external.opensearch.backup]
        enable = true
        location = "s3"

      [global.v1.external.opensearch.backup.s3]

        # bucket (required): The name of the bucket
        bucket = "bucket-name"

        # base_path (optional): The path within the bucket where backups should be stored
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
        endpoint = "<Your Object Storage URL>"

        # base_path (optional): The path within the bucket where backups should be stored
        # If base_path is not set, backups will be stored at the root of the bucket.
        base_path = "automate"

      [global.v1.backups.s3.credentials]
        access_key = "<Your Access Key>"
        secret_key = "<Your Secret Key>"
    ```

3. Execute the command given below to trigger the deployment.

    ```sh
    ./chef-automate config patch --frontend /path/to/automate.toml
    ```

## Backup and Restore Commands

### Backup

- To create the backup, by running the backup command from bastion. The backup command is as shown below:

    ```cmd
    chef-automate backup create
    ```

<!-- ### Restore

This section includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using File System.

The restore operation restores all the data while the backup is going on. The restore operation stops will the ongoing backup procedure. Let's understand the whole process by a scenario:

-   Create a automate _UserA_ and generate an API token named _Token1_ for _UserA_.
-   Create a backup, and let's assume the back id to be _20220708044530_.
-   Create a new user _UserB_ and a respective API token named _Token2_.
-   Now, suppose you want to restore data in the same automate cluster. In that case, the data will only be stored for _UserA_ with its token as the backup bundle only contains the _UserA_, and the _UserB_ is not available in the backup bundle. -->

#### Restoring the Backed-up Data from Object Storage

To restore backed-up data of the Chef Automate High Availability (HA) using External Object Storage, follow the steps given below:

- Check the status of Automate HA Cluster from the bastion nodes by executing the `chef-automate status` command.

- Execute the restore command from bastion`chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key"`.

- In case of Airgapped Environment, Execute this restore command from bastion `chef-automate backup restore <object-storage-bucket-path>/backups/BACKUP_ID --skip-preflight --airgap-bundle </path/to/bundle>`.

{{< note >}}

- If you are restoring the backup from an older version, then you need to provide the `--airgap-bundle </path/to/current/bundle>`.
- If you have not configured S3 access and secret keys during deployment or if you have taken backup on a diffrent bucket, then you need to provide the `--s3-access-key <Access_Key>` and `--s3-secret-key <Secret_Key>` flags.

{{< /note >}}

## Troubleshooting

While running the restore command, If it prompts any error follow the steps given below.

- Check the chef-automate status in Automate node by running `chef-automate status`.
- Also check the hab svc status in automate node by running `hab svc status`.
- If the deployment services is not healthy then reload it using `hab svc load chef/deployment-service`.
- Now, check the status of Automate node and then try running the restore command from bastion.
