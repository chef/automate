+++
title = "AWS Deployment using S3"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment using S3"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore AWS Deployment - S3"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 240
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< note >}}

-   If the user chooses `backup_config` as `s3` in `config.toml,` backup is already configured during deployment, **the below steps are not required**. If we have kept the `backup_config` blank, then the configuration needs to be configured manually.

{{< /note >}}

### Overview

To Communicate with Amazon S3 we need an IAM Role with the required [policy](/automate/backup/#aws-s3-permissions).

Attach the IAM Role to the All the OpenSearch Node and Frontend Node.

<!--
**Permissions Required**

Check if the IAM user has all the required permissions. The permission policies are listed below:

1. AdministratorAccess

1. S3FullAccess (for aws AmazonS3FullAccess)

Create an IAM role to give access to **S3** in **OpenSearch** instances. The role should already be assigned as the OpenSearch instance tries to communicate S3.

The permissions can either be directly added to the user or added via **IAM Group**.

Once done with the above steps, `.toml` file and patch the `.config`. In the file, modify the values listed below:
-->

{{< note >}}

In case of if you are using the Managed AWS Service you need to create a [snapshot-role](/automate/managed_services/#opensearch-setup) for OpenSearch.

{{< /note >}}

#### Configuration in Provision host

1. Create a .toml say, `automate.toml`.

-   Refer to the content for the `automate.toml` file below:

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
        access_key = "<Your Access Key>"
        secret_key = "<Your Seecret Key>"
    ```

-   Execute the command given below to trigger the deployment.

    ```sh
    chef-automate config patch --frontend automate.toml
    ```

{{< note >}} **IAM Role:** Assign the IAM Role to all the OpenSearch instances in the cluster created above. {{< /note >}}

## Backup and Restore Commands

### Backup

-   To create the backup, by running the backup command from bastion. The backup command is as shown below:

    ```cmd
    chef-automate backup create
    ```

#### Restoring the Backed-up Data from Object Storage

To restore backed-up data of the Chef Automate High Availability (HA) using External AWS S3, follow the steps given below:

-   Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.


-   Log in to the same instance of Chef Automate front-end node from which backup is taken.

-   Execute the restore command from bastion `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key"`.

-   In case of Airgapped Environment, Execute this restore command from bastion `chef-automate backup restore <object-storage-bucket-path>/backups/BACKUP_ID --airgap-bundle </path/to/bundle> --skip-preflight`.

## Troubleshooting

While running the restore command, If it prompts any error follow the steps given below.

-  check the chef-automate status in Automate node by running `chef-automate status`.
-  Also check the hab svc status in automate node by running `hab svc status`.
-  If the deployment services is not healthy then reload it using `hab svc load chef/deployment-service`.
-  Now, check the status of Automate node and then try running the restore command from bastion.

For **Disaster Recovery or AMI upgarde**, while running the restore in secondary cluster which is in different region follow the steps given below.

-  Make a curl request in any opensearch node`curl -XGET https://localhost:9200/_snapshot?pretty --cacert /hab/svc/automate-ha-opensearch/config/certificates/root-ca.pem --key /hab/svc/automate-ha-opensearch/config/certificates/admin-key.pem --cert /hab/svc/automate-ha-opensearch/config/certificates/admin.pem -k`
-  check the curl request response if the region is not matching with the primary cluster follow the below steps:
1. Modify the region in FrontEnd nodes by patching the below configs with command, `chef-automate config patch <file-name>.toml --fe`

```cmd
[global.v1.external.opensearch.backup.s3.settings]
              region = "<FIRST-CLUSTER-REGION>"
```

2. Make a PUT request in an Opensearch node by running this script:

```cmd
indices=(
chef-automate-es6-automate-cs-oc-erchef
chef-automate-es6-compliance-service
chef-automate-es6-event-feed-service
chef-automate-es6-ingest-service
)
for index in ${indices[@]}; do
curl -XPUT -k -H 'Content-Type: application/json' https://<IP>:9200/_snapshot/$index --data-binary @- << EOF
{
  "type" : "s3",
    "settings" : {
      "bucket" : "<YOUR-PRIMARY-CLUSTER-BUCKET-NAME>",
      "base_path" : "elasticsearch/automate-elasticsearch-data/$index",
      "region" : "<YOUR-PRIMARY-CLUSTER-REGION>",
      "role_arn" : " ",
      "compress" : "false"
    }
}
EOF
done

```