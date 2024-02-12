Try these steps if Chef Automate returns an error while restoring data.

1. Check the Chef Automate status.

   ```sh
   chef-automate status
   ```

1. Check the status of your Habitat service on the Automate node.

   ```sh
   hab svc status
   ```

1. If the deployment services are not healthy, reload them.

    ```sh
    hab svc load chef/deployment-service
    ```

Now check the status of the Automate node and then try running the restore command from the bastion host.

1. How to change the `base_path` or `path`. The steps for the File System backup are as shown below:

   - While at the time of deployment `backup_mount` default value will be `/mnt/automate_backups`
   - In case, if you modify the `backup_mount` in `config.toml` before deployment, then the deployment process will do the configuration with the updated value
   - In case, you changed the `backup_mount` value post-deployment, then we need to patch the configuration manually to all the frontend and backend nodes, for example, if you change the `backup_mount` to `/bkp/backps`
   - Update the FE nodes with the below template, use the command  `chef-automate config patch fe.toml --fe`

      ```sh
         [global.v1.backups]
            [global.v1.backups.filesystem]
               path = "/bkp/backps"
         [global.v1.external.opensearch.backup]
            [global.v1.external.opensearch.backup.fs]
               path = "/bkp/backps"
      ```

   - Update the OpenSearch node with the below template, use the command  `chef-automate config patch os.toml --os`

      ```sh
      [path]
         repo = "/bkp/backps"
      ```

      - Run the curl request to one of the automate frontend node

        ```sh
        curl localhost:10144/_snapshot?pretty
        ```

         - If the response is empty `{}`, then we are good

         - If the response has json output, then it should have correct value for the `backup_mount`, refer the `location` value in the response. It should start with the `/bkp/backps`

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

         - If the pre string in the `location` is not match with `backup_mount`, then we need to to delete the existing snapshots. use below script to delete the snapshot from the one of the automate frontend node.

         ```sh
            snapshot=$(curl -XGET http://localhost:10144/_snapshot?pretty | jq 'keys[]')
            for name in $snapshot;do
	            key=$(echo $name | tr -d '"')
               curl -XDELETE localhost:10144/_snapshot/$key?pretty
            done
         ```

         - The above scritp requires the `jq` needs to be installed, You can install from the airgap bundle, please use command on the one of the automate frontend node to locate the `jq` package.

         ```sh
         ls -ltrh /hab/cache/artifacts/ | grep jq

         -rw-r--r--. 1 ec2-user ec2-user  730K Dec  8 08:53 core-jq-static-1.6-20220312062012-x86_64-linux.hart
         -rw-r--r--. 1 ec2-user ec2-user  730K Dec  8 08:55 core-jq-static-1.6-20190703002933-x86_64-linux.hart
         ```

         - In case of multiple `jq` version, then install the latest one. use the below command to install the `jq` package to the automate frontend node

         ```sh
         hab pkg install /hab/cache/artifacts/core-jq-static-1.6-20190703002933-x86_64-linux.hart -bf
         ```

1. Below steps for object storage as a backup option

      - While at the time of deployment `backup_config` will be `object_storage`
      - To use the `object_storage`, we are using below template at the time of deployment

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

      - If you configured pre deployment, then we are good
      - If you want to change the `bucket` or `base_path`, then use the below template for Frontend nodes

      ```sh
      [global.v1]
        [global.v1.external.opensearch.backup.s3]
            bucket = "<BUCKET_NAME>"
            base_path = "opensearch"
         [global.v1.backups.s3.bucket]
            name = "<BUCKET_NAME>"
            base_path = "automate"
      ```

      - You can choose any value for the variable `base_path`. `base_path` patch is only required for the frontend node.
      - Use the command to apply the above template `chef-automate config patch frontend.toml --fe`
      - Post the configuration patch, and use the curl request to validate

         ```sh
         curl localhost:10144/_snapshot?pretty
         ```

      - If the response is empty `{}`, then we are good

      - If the response has JSON output, then it should have the correct value for the `base_path`

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

         - In case of `base_path` value is not matching, then we have to delete the existing `snapshot`. please refer to the steps from the file system.
