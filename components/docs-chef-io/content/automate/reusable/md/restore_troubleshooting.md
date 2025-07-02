Follow the steps below if Chef Automate encounters an error during data restoration.

1. Check the Chef Automate status.

   ```sh
   chef-automate status
   ```

2. Check the status of your Habitat service on the Automate node.

   ```sh
   hab svc status
   ```

3. If the deployment services are not healthy, reload them.

    ```sh
    hab svc load chef/deployment-service
    ```

4. Check the status of the Automate node, and then attempt to run the restore command from the bastion host.

5. To change the `base_path` or `path`, follow the steps below for performing a backup.

   * File System

      * During deployment, the `backup_mount` is default set to `/mnt/automate_backups`.
      * The deployment process will automatically apply the updated path if you update the `backup_mount` value in the `config.toml` file before deployment.
      * If the `backup_mount` value is changed after deployment (e.g., to /bkp/backps), you must manually patch the configuration on all frontend and backend nodes.
      * Update the FE nodes using the template below. To update the configuration, use the command  `chef-automate config patch fe.toml --fe`.

      ```sh
         [global.v1.backups]
            [global.v1.backups.filesystem]
               path = "/bkp/backps"
         [global.v1.external.opensearch.backup]
            [global.v1.external.opensearch.backup.fs]
               path = "/bkp/backps"
      ```

      * Update the OpenSearch nodes using the template provided below. Use the `chef-automate config patch os.toml --os` command to update the Opensearch node configs.

      ```sh
      [path]
         repo = "/bkp/backps"
      ```

      * Run the curl request against one of the Automate frontend nodes.

        ```sh
        curl localhost:10144/_snapshot?pretty
        ```

         * If the response is an empty JSON object `{}`, no changes are required to the snapshot settings in the OpenSearch cluster.

         * If you see a JSON response similar to the example below, check that the `backup_mount` setting is correctly configured. Use the `location` value in the response to verify. It should start with `/bkp/backps`.

        ```sh
        {
         "chef-automate-es6-event-feed-service" : {
         "type" : "fs",
         "settings" : {
         "location" : "/mnt/automate_backups/opensearch/automate-elasticsearch-data/chef-automate-es6-event-feed-service"
               }
            },
         "chef-automate-es6-compliance-service" : {
         "type" : "fs",
         "settings" : {
         "location" : "/mnt/automate_backups/opensearch/automate-elasticsearch-data/chef-automate-es6-compliance-service"
               }
            },
         "chef-automate-es6-ingest-service" : {
             "type" : "fs",
         "settings" : {
         "location" : "/mnt/automate_backups/opensearch/automate-elasticsearch-data/chef-automate-es6-ingest-service"
               }
            },
         "chef-automate-es6-automate-cs-oc-erchef" : {
         "type" : "fs",
         "settings" : {
         "location" : "/mnt/automate_backups/opensearch/automate-elasticsearch-data/chef-automate-es6-automate-cs-oc-erchef"
               }
            }
         }
        ```

         * If the prefix in the `location` value does not match the `backup_mount`, the existing snapshots must be deleted. Use the script below to delete the snapshots from the one of the Automate frontend nodes.

         ```sh
            snapshot=$(curl -XGET http://localhost:10144/_snapshot?pretty | jq 'keys[]')
            for name in $snapshot;do
	            key=$(echo $name | tr -d '"')
               curl -XDELETE localhost:10144/_snapshot/$key?pretty
            done
         ```

         * The above script requires `jq` to be installed, You can install it from the airgap bundle. To locate the `jq` package, run the command below on one of the Automate frontend nodes.

         ```sh
         ls -ltrh /hab/cache/artifacts/ | grep jq

         -rw-r--r--. 1 ec2-user ec2-user  730K Dec  8 08:53 core-jq-static-1.6-20220312062012-x86_64-linux.hart
         -rw-r--r--. 1 ec2-user ec2-user  730K Dec  8 08:55 core-jq-static-1.6-20190703002933-x86_64-linux.hart
         ```

         * If multiple versions of `jq` are available, install the latest one. Use the command below to install the `jq` package on one of the Automate frontend nodes.

         ```sh
         hab pkg install /hab/cache/artifacts/core-jq-static-1.6-20190703002933-x86_64-linux.hart -bf
         ```

   * Object Storage

      * During deployment, the `backup_config` should be set to `object_storage`.
      * To use `object_storage`, we use the following template during deployment.

      ```sh
         [object_storage.config]
          google_service_account_file = ""
          location = ""
          bucket_name = ""
          access_key = ""
          secret_key = ""
          endpoint = ""
          region = ""
      ```

      * If you configured it before deployment, then you are all set.
      * If you want to change the `bucket` or `base_path`, use the following template for Frontend nodes.

      ```sh
      [global.v1]
        [global.v1.external.opensearch.backup.s3]
            bucket = "<BUCKET_NAME>"
            base_path = "opensearch"
         [global.v1.backups.s3.bucket]
            name = "<BUCKET_NAME>"
            base_path = "automate"
      ```

      * You can assign any value to the `base_path` variable. The `base_path` configuration is required only for the Frontend nodes.
      * Use the command `chef-automate config patch frontend.toml --fe` to apply the above template and update the configuration.
      * Use the following curl request to validate the configuration.

         ```sh
         curl localhost:10144/_snapshot?pretty
         ```

      * If the response is an empty JSON object ({}), the configuration is valid.

      * If the response contains a JSON output similar to the example below, it should have the correct value for the `base_path`.

         ```sh
         {
             "chef-automate-es6-event-feed-service" : {
               "type" : "s3",
               "settings" : {
                 "bucket" : "MY-BUCKET",
                 "base_path" : "opensearch/automate-elasticsearch-data/chef-automate-es6-event-feed-service",
                 "readonly" : "false",
                 "compress" : "false"
               }
             },
             "chef-automate-es6-compliance-service" : {
               "type" : "s3",
               "settings" : {
                 "bucket" : "MY-BUCKET",
                 "base_path" : "opensearch/automate-elasticsearch-data/chef-automate-es6-compliance-service",
                 "readonly" : "false",
                 "compress" : "false"
               }
             },
             "chef-automate-es6-ingest-service" : {
               "type" : "s3",
               "settings" : {
                 "bucket" : "MY-BUCKET",
                 "base_path" : "opensearch/automate-elasticsearch-data/chef-automate-es6-ingest-service",
                 "readonly" : "false",
                 "compress" : "false"
               }
             },
             "chef-automate-es6-automate-cs-oc-erchef" : {
               "type" : "s3",
               "settings" : {
                 "bucket" : "MY-BUCKET",
                 "base_path" : "opensearch/automate-elasticsearch-data/chef-automate-es6-automate-cs-oc-erchef",
                 "readonly" : "false",
                 "compress" : "false"
               }
             }
         }
         ```

         * If the `base_path` value does not match, you must delete the existing snapshots. Please take a look at the File System troubleshooting steps for guidance.
