+++
title = "Prerequisites"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Prerequisites"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Prerequisites"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 210
+++

This page explains the prerequisites of the AWS and on-premise backed backup. You can also take back up for:

- External Elastic Search (ES) and Postgres-SQL on Amazon S3 bucket.
- External Elastic Search (ES) and Postgres-SQL on External File-system (EFS). You can take backup on EFS system through DNS or IP.

## AWS Backed Backup

The two pre-backup configurations for AWS are:

- For s3 Backup
- For EFS Backup

### Pre Backup Configuration for s3 Backup

In order to run the terraform scripts, the IAM users should have proper permissions. Going forward we will also discuss about the required permissions. You should have your  `secret access key` and `key id` or else you can regenerate a new access key.

#### Permissions Required

Check if the IAM user has all the required permissions. The permission policies are listed below:

1. AdministratorAccess

1. APIGatewayAdministrator (For aws AmazonAPIGatewayAdministrator)

1. S3FullAccess (for aws AmazonS3FullAccess)

Create an IAM role to give access of **s3** to **ElasticSearch** instances. The role should already be assigned as the ElasticSearch instance tries to communicate s3.

The permissions can either be directly added to the user or can be added via **IAM Group**.

Once done with the above steps, `.toml` file and patch the `.config`. In the file, modify the values listed below:

1. **bucket name (bucket = "bucket-name" and name = "bucket-name")**

2. `mkdir configs`

3. `vi configs/automate.toml`

The content for the `automate.toml` file is given below:

```sh
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
    access_key = "AKIARUQHMSKV6BXLAXHO"
    secret_key = "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"
```

Execute the command given below to trigger the deployment.

```sh
./chef-automate config patch configs/automate.toml
```

Back-up the configurations once the cluster has been deployed.

{{< note >}} **IAM Role:** Assign the IAM Role to the all the ElasticSearch instances in the cluster created above. {{< /note >}}

### File System(EFS) Configuration for backup


Backup on share file system. (This section is specific for aws).

Create the EFS over the AWS.

Once EFS is ready there are 2 ways to mount (via DNS and via IP).

Open the port(2049) Proto(NFS) for EFS security group.













## On-Premise Backed Backup

### Pre-backup configuration and setup for File system backup

A shared file system is needed to create Elasticsearch snapshots. In order to register the snapshot repository with Elasticsearch it is necessary to mount the same shared filesystem to the same location on all master and data nodes. This location (or one of its parent directories) must be registered in the path.repo setting on all master and data nodes.

Assuming that the shared filesystem is mounted to /mnt/automate_backups, we can configure Automate to register the snapshot locations with Elasticsearch.

Ensure the shared file system is mounted on all Elasticsearch servers:
      mount /mnt/automate_backups
Create elasticsearch sub-directory and set permissions, this will only need to be done on a single Elasticsearch server if the network mount is correctly mounted.

      sudo mkdir /mnt/automate_backups/elasticsearch
      sudo chown hab:hab /mnt/automate_backups/elasticsearch/
Configure Elasticsearch path.repo setting by SSHing to a single Elasticsearch server and using the following steps:

Export the current Elasticsearch config from the Habitat supervisor. You will need to have root access to run the following commmands
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-ha-elasticsearch | tail -n +2 > es_config.toml
Edit es_config.toml and add the following settings to the end of the file.
Note: If credentials have never been rotated this file may be empty.
   [es_yaml.path]   
   # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb   
   repo = "/mnt/automate_backups/elasticsearch" 
Apply updated es_config.toml config to Elasticsearch, this only needs to be done once. This will trigger a restart of the Elasticsearch services on each server.
hab config apply automate-ha-elasticsearch.default $(date '+%s') es\_config.toml

hab svc status (check elasticsearch service is up or not)

curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check es. Check that all the indices is green or not)

# Watch for a message about Elasticsearch going from RED to GREEN
`journalctl -u hab-sup -f | grep 'automate-ha-elasticsearch'
Configure Automate to handle external Elasticsearch backups

Create a automate.toml file on the provisioning server

touch automate.toml

Add the following configuration to automate.toml on the provisioning host.

   [global.v1.external.elasticsearch.backup]
   enable = true
   location = "fs"

   [global.v1.external.elasticsearch.backup.fs]
   # The `path.repo` setting you've configured on your Elasticsearch nodes must be
   # a parent directory of the setting you configure here:
   path = "/mnt/automate_backups/elasticsearch"

   [global.v1.backups.filesystem]
   path = "/mnt/automate_backups/backups"
After that patch the config. This will trigger the deployment also.

./chef-automate config patch automate.toml

### Pre-backup configuration for Object storage (Non AWS)

This section provide pre-backup configuration required in case we plan to backup our data on object storage system(Other than AWS S3) like Minio, non AWS S3.

A) Steps to set key/secret using commands mentioned below :

Login to all the elastic-search nodes and perform below steps on all the ES nodes.

1.1 export ES_PATH_CONF="/hab/svc/automate-ha-elasticsearch/config"

1.2 hab pkg exec chef/elasticsearch-odfe elasticsearch-keystore add s3.client.default.access_key (It will ask to enter key, please enter your key)

1.3 hab pkg exec chef/elasticsearch-odfe elasticsearch-keystore add s3.client.default.secret_key (It will ask to enter secret, please enter your key)

1.4 chown hab:hab /hab/svc/automate-ha-elasticsearch/config/elasticsearch.keystore (Setting hab:hab permission)

1.5 curl -k -X POST "https://127.0.0.1:9200/_nodes/reload_secure_settings?pretty" -u admin:admin (Command to load the above setting)

After running command 1.5 on 3rd node, this will be the final output-

{
  "_nodes" : {
    "total" : 3,
    "successful" : 3,
    "failed" : 0
  },
  "cluster_name" : "chef-insights",
  "nodes" : {
    "lenRTrZ1QS2uv_vJIwL-kQ" : {
      "name" : "lenRTrZ"
    },
    "Us5iBo4_RoaeojySjWpr9A" : {
      "name" : "Us5iBo4"
    },
    "qtz7KseqSlGm2lEm0BiUEg" : {
     "name" : "qtz7Kse"
    }
  }
}
B) To override the existing default endpoint:

Login to one of the elastic search instance and run the below command on that (You will need root access to run the command):
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-ha-elasticsearch | tail -n +2 > es_config.toml
Edit the created es_config.toml file and add the following settings to the end of the file. Note: If credentials have never been rotated this file may be empty.
[es_yaml.s3.client.default]
 endpoint = "<Bloomberg S3 endpoint, e.g. bloomberg.s3.com>"
Use below command to apply the updated es_config.toml changes, this only needs to be done once: Note: This will trigger a restart of the Elasticsearch services on each server.
hab config apply automate-ha-elasticsearch.default $(date '+%s') es_config.toml
After that run command :
journalctl -u hab-sup -f | grep 'automate-ha-elasticsearch'
And watch for a message about Elasticsearch going from RED /YELLOW to GREEN.


