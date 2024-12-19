+++
title = "Back Up On-Prem Deployment With Object Storage"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-Prem Back Up With Object Storage"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore Object Storage"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 220
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

This document shows how to configure, back up, and restore a Chef Automate high availability deployment with object storage.

During deployment of Chef Automate, if you set `backup_config = "object_storage"` or `backup_config = "file_system"` in the Automate configuration TOML file, then backup is already configured and you don't need to configure data backup for Chef Automate.
If a backup wasn't configured during the initial deployment, then follow these instructions to configure it manually.

Chef Automate supports backing up data to the following platforms:

- S3 (AWS S3, MinIO, non-AWS S3)
- Google Cloud Storage (GCS)

## Configure backup for S3

This section shows how to configure data back up on a Chef Automate high availability deployment to object storage on AWS S3, MinIO, or non-AWS S3.

### Configure OpenSearch nodes

Add a secret key and access key for your S3 backup provider on every OpenSearch node.

{{< note >}}

Encrypted S3 buckets are supported only with Amazon S3 managed keys (SSE-S3).

{{< /note >}}

1. Set the OpenSearch path configuration location.

    ```sh
    export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"
    ```

1. Add your S3 access and secret keys to the OpenSearch keystore.

    ```sh
    hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.access_key
    hab pkg exec chef/automate-ha-opensearch opensearch-keystore add s3.client.default.secret_key
    ```

1. Change ownership of the keystore.

    ```sh
    chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore
    ```

1. Load the secure settings into the OpenSearch keystore.

    ```sh
    curl -X POST https://localhost:9200/_nodes/reload_secure_settings?pretty --cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem --key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem --cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem -k
    ```

1. Repeat these steps on all OpenSearch nodes until they are all updated.

#### OpenSearch health check

{{< readfile file="content/automate/reusable/md/opensearch_health_check.md" >}}

### Patch the Automate configuration

On the bastion host, update the S3 and OpenSearch configuration.

Before starting, make sure the frontend nodes and OpenSearch nodes have access to the object storage endpoint.

1. Create a TOML file on the bastion host with the following settings.

    ```sh
    [s3]
      [s3.client.default]
        protocol = "https"
        read_timeout = "60s"
        max_retries = "3"
        use_throttle_retries = true
        endpoint = "s3.example.com"
    ```

    Replace the value of `endpoint` with the URL of your S3 storage endpoint.

1. Add the following content to the TOML file to configure OpenSearch.

    ```sh
    [global.v1]
      [global.v1.external.opensearch.backup]
        enable = true
        location = "s3"

      [global.v1.external.opensearch.backup.s3]

        # bucket (required): The name of the bucket
        bucket = "<BUCKET_NAME>"

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
        name = "<BUCKET_NAME>"

        # endpoint (required): The endpoint for the region the bucket lives in for Automate Version 3.x.y
        # endpoint (required): For Automate Version 4.x.y, use this https://s3.amazonaws.com
        endpoint = "<OBJECT_STORAGE_URL>"

        # base_path (optional): The path within the bucket where backups should be stored
        # If base_path is not set, backups will be stored at the root of the bucket.
        base_path = "automate"

      [global.v1.backups.s3.credentials]
        access_key = "<ACCESS_KEY>"
        secret_key = "<SECRET_KEY>"
    ```

1. Use the `patch` subcommand to patch the Automate configuration.

    ```sh
    ./chef-automate config patch --frontend /PATH/TO/FILE_NAME.TOML
    ```

## Configure backup on Google Cloud Storage

This sections shows how to configure a Chef Automate high availability deployment to back up data to object storage on Google Cloud Storage (GCS).

### Configure OpenSearch nodes

Add a GCS service account file that gives access to the GCS bucket to every OpenSearch node.

1. Log in to an OpenSearch node and set the OpenSearch path and GCS service account file locations.

    ```sh
    export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"
    export GCS_SERVICE_ACCOUNT_JSON_FILE_PATH="/PATH/TO/GOOGLESERVICEACCOUNT.JSON"
    ```

1. Change ownership of the GCS service account file.

    ```sh
    chown -RL hab:hab $GCS_SERVICE_ACCOUNT_JSON_FILE_PATH
    ```

1. Add the GCS service account file to OpenSearch.

    ```sh
    hab pkg exec chef/automate-ha-opensearch opensearch-keystore add-file --force gcs.client.default.credentials_file $GCS_SERVICE_ACCOUNT_JSON_FILE_PATH
    ```

1. Change ownership of the keystore.

    ```sh
    chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore
    ```

1. Load the secure settings into the OpenSearch keystore.

    ```sh
    curl -X POST https://localhost:9200/_nodes/reload_secure_settings?pretty --cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem --key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem --cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem -k
    ```

1. Repeat these steps on all OpenSearch nodes until they are all updated.

After updating all nodes, the above curl command will return an output similar to this:

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

#### OpenSearch health check

{{< readfile file="content/automate/reusable/md/opensearch_health_check.md" >}}

### Patch the Automate configuration

On the bastion host, update the OpenSearch configuration.

Before starting, make sure the frontend nodes and OpenSearch nodes have access to the object storage endpoint.

1. Create a TOML file on the bastion host with the following settings.

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

        # endpoint = ""

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

1. Patch the Automate configuration to trigger the deployment.

    ```sh
    ./chef-automate config patch --frontend /PATH/TO/FILE_NAME.TOML
    ```

## Backup and Restore

### Backup

To create a backup, run the backup command from the bastion host.

```sh
chef-automate backup create
```

### Restore

Restore a backup from external object storage.

1. Check the status of the Automate HA cluster from the bastion host.

    ```sh
    chef-automate status
    ```

1. Restore the backup by running the restore command from the bastion host.

   For S3:

   ```sh
   chef-automate backup restore s3://BUCKET_NAME/PATH/TO/BACKUPS/BACKUP_ID --skip-preflight --s3-access-key "ACCESS_KEY" --s3-secret-key "SECRET_KEY"
   ```

   For GCS:

   ```sh
   chef-automate backup restore gs://BUCKET_NAME/PATH/TO/BACKUPS/BACKUP_ID --gcs-credentials-path "PATH/TO/GOOGLE_SERVICE_ACCOUNT.JSON"
   ```

   In an airgapped environment:

   ```sh
   chef-automate backup restore <OBJECT-STORAGE-BUCKET-PATH>/BACKUPS/BACKUP_ID --skip-preflight --airgap-bundle </PATH/TO/BUNDLE>
   ```

#### Troubleshooting

{{< readfile file = "content/automate/reusable/md/restore_troubleshooting.md" >}}
