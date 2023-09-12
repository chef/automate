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

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

{{< note >}}

- If the user chooses `backup_config` as `object_storage` or `file_system` in `config.toml`, backup is already configured during the deployment, and in that case **the below steps are not required**. If `backup_config` is left blank, then the configuration needs to be configured manually.
- Encrypted S3 bucket are supported with only Amazon S3 managed keys (SSE-S3).

{{< /note >}}

## Overview

This section provides the pre-backup configuration required to back up the data on the Object Storage System. The supported Object storage could be of two types:
  - S3 (AWS S3, Minio, Non-AWS S3)
  - GCS

The value of location varies based on the type of Object Storage, that is, `location="s3"` incase of S3 storage that includes AWS S3, Minio and Non-AWS S3, and `location="gcs"` incase of Google Cloud Storage bucket.



### Configuration in OpenSearch Node 

#### For _Minio_, _Non-AWS S3_, _AWS S3_
For the Object storages like _Minio_, _Non-AWS S3_, _AWS S3_, the following are the steps to set a secret key and access key of the bucket on the OpenSearch nodes:

1. Log in to **all** the OpenSearch nodes and execute the following commands one after another.

- `export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"`
- `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.access_key` (When asked, Enter your key)
- `hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.secret_key` (When asked, Enter your key/secret)
- `chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore` (Setting hab:hab permission)
- `curl -k -X POST "https://127.0.0.1:9200/_nodes/reload_secure_settings?pretty" -u admin:admin` (Command to load the above setting)

#### For _GCS_
For GCP Object Storage, the following are the steps to set access to the GCS bucket on the OpenSearch nodes:

1. Log in to **all** the OpenSearch nodes and execute the following commands.

- `export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"`
- `export GCS_SERVICE_ACCOUNT_JSON_FILE_PATH="/path/to/googleServiceAccount.json"` (Provide the file path of your googleServiceAccount.json file. Note that the content of this json file should look like the snippet depicted [here](#structure-of-google-service-account-json-file))
- `chown -RL hab:hab $GCS_SERVICE_ACCOUNT_JSON_FILE_PATH`
- `hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore add-file --force gcs.client.default.credentials_file $GCS_SERVICE_ACCOUNT_JSON_FILE_PATH`
- `chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore` (Setting hab:hab permission)
- `curl -k -X POST "https://127.0.0.1:9200/_nodes/reload_secure_settings?pretty" -u admin:admin` (Command to load the above setting)

The final output after running the above curl command on all nodes is given below:

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

#### Configuration for OpenSearch Node from Bastion Host (only for S3)

1. To override the existing default endpoint:

- Create a `.toml` file on **the provisioning server** using the following command:

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

This will update the configuration in the Opensearch node.

#### Healthcheck commands

- The following command can be run from the OpenSearch node

    ```sh
    hab svc status (check whether OpenSearch service is up or not)

    curl -k -X GET "https://localhost:9200/_cat/indices/*?v=true&s=index&pretty" -u admin:admin (Another way to check is to check whether all the indices are green or not)

    # Watch for a message about OpenSearch going from RED to GREEN
    `journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```

### Configuration for Automate Node from Bastion Host

{{< note >}}

Make sure all the frontend nodes and OpenSearch have access to the object storage.

{{< /note >}}

Once done with the OpenSearch setup, add the following `automate.toml` file and patch the updated config to all frontend nodes. In the file, modify the values listed below:

1. Create .toml file by `vi automate.toml`

2. Refer to the content for the `automate.toml` file below for S3:

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

3. Refer to the content for the `automate.toml` file below for GCS:


    ```sh
    [global.v1]
      [global.v1.external.opensearch.backup]
        enable = true
        location = "gcs" 

      [global.v1.external.opensearch.backup.gcs]

        # bucket (required): The name of the bucket
        bucket = "bucket-name"

        # base_path (optional): The path within the bucket where backups should be stored
        # If base_path is not set, backups will be stored at the root of the bucket.
        base_path = "opensearch"
        client = "default"

      [global.v1.backups]
        location = "gcs"

      [global.v1.backups.gcs.bucket]
        # name (required): The name of the bucket
        name = "bucket-name"

        endpoint = "<Your Object Storage URL>"

        # base_path (optional): The path within the bucket where backups should be stored
        # If base_path is not set, backups will be stored at the root of the bucket.
        base_path = "automate"

      [global.v1.backups.gcs.credentials]
        json = '''{
          "type": "service_account",
          "project_id": "chef-automate-ha",
          "private_key_id": "7b1e77baec247a22a9b3****************f",
          "private_key": "<PRIVATE KEY>",
          "client_email": "myemail@chef.iam.gserviceaccount.com",
          "client_id": "1******************1",
          "auth_uri": "https://accounts.google.com/o/oauth2/auth",
          "token_uri": "https://oauth2.googleapis.com/token",
          "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
          "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/myemail@chef.iam.gserviceaccount.com",
          "universe_domain": "googleapis.com"
        }'''
    ```

3. Execute the command given below to trigger the deployment.

    ```sh
    ./chef-automate config patch --frontend /path/to/automate.toml
    ```

#### Structure of Google Service Account JSON file

  ```json
  {
    "type": "service_account",
    "project_id": "chef-automate-ha",
    "private_key_id": "7b1e77baec247a22a9b3****************f",
    "private_key": "<PRIVATE KEY>",
    "client_email": "myemail@chef.iam.gserviceaccount.com",
    "client_id": "1******************1",
    "auth_uri": "https://accounts.google.com/o/oauth2/auth",
    "token_uri": "https://oauth2.googleapis.com/token",
    "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
    "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/myemail@chef.iam.gserviceaccount.com",
    "universe_domain": "googleapis.com"
  }
  ```

## Backup and Restore Commands

### Backup

- To create the backup, run the backup command from Bastion. The backup command is as shown below:

    ```cmd
    chef-automate backup create
    ```


#### Restoring the Backed-up Data from Object Storage

To restore backed-up data of the Chef Automate High Availability (HA) using External Object Storage, follow the steps given below:

- Check the status of the Automate HA Cluster from the Bastion nodes by executing the `chef-automate status` command.

- For S3, execute the following command from the Bastion to restore `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key"`.

- For GCS, execute the following command from the Bastion to restore `chef-automate backup restore gs://bucket_name/path/to/backups/BACKUP_ID --gcs-credentials-path "path/to/googleServiceAccount.json/file"`.

- In the case of Airgapped Environment, Execute this restore command from the Bastion `chef-automate backup restore <object-storage-bucket-path>/backups/BACKUP_ID --skip-preflight --airgap-bundle </path/to/bundle>`.

{{< note >}}

- If you are restoring the backup from an older version, then you need to provide the `--airgap-bundle </path/to/current/bundle>`.
- If you have not configured S3 access and secret keys during deployment or if you have taken backup on a different bucket, then you need to provide the `--s3-access-key <Access_Key>` and `--s3-secret-key <Secret_Key>` flags.

{{< /note >}}

## Troubleshooting

While running the restore command, If it prompts any error follow the steps given below.

- Check the chef-automate status in Automate node by running `chef-automate status`.
- Also check the hab svc status in the Automate node by running `hab svc status`.
- If the deployment services are not healthy then reload it using `hab svc load chef/deployment-service`.
- Now, check the status of the Automate node and then try running the restore command from Bastion.
