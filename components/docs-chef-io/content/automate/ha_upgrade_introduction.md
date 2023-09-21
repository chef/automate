+++
title = "Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Upgrade"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_upgrade_introduction.md Upgrade HA"
    weight = 70
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Steps to upgrade the Chef Automate HA are as shown below:

- Download the latest CLI 
  ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

- Download Airgapped Bundle, download latest Bundle with this:

  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
  ```
  Download specific version bundle with this:
  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/<version>.aib -o automate-<version>.aib
  ```

  {{< note >}} 
  Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
  {{< /note >}}

- If we want to only upgrade FrontEnd Services i.e. Chef Automate and Chef Infra Server.
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends
  ```

- If we want to only upgrade BackEnd Services i.e. Postgresql and OpenSearch.
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends
  ```

- To upgrade full Chef Automate HA System run this command from Bastion Host: 
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib
  ```


{{< note >}}

  - BackEnd upgrades will restart the backend service, which take time for cluster to be in health state.
  - Upgrade command, currently only supports minor upgrade.  
{{< /note >}}

- To skip user confirmation prompt in upgrade, you can pass a flag
  ```bash 
    chef-automate upgrade run --airgap-bundle latest.aib --auto-approve
    OR 
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends --auto-approve
    OR
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends --auto-approve
  ```

Upgrade will also check for new version of bastion workspace, if new version is available, it will prompt for a confirmation for workspace upgrade before upgrading the Frontend or backend nodes, 

In case of yes, it will do workspace upgrade and no will skip this.
We can also pass a flag in upgrade command to avoid prompt for workspace upgrade. 

  ```bash
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade yes
      OR  
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade no
  ```

{{< note >}}

  AMI Upgrade is only for AWS deployment, as in On-Premise Deployment all the resources are managed by the customers themselves.  

{{< /note >}}

## AMI Upgrade Setup For AWS Deployment

{{< note >}}

  In this following section, the old cluster with older AMI Images is referred as the **Primary Cluster** and the cluster which has upgraded AMI is referred as the **New Cluster**.

{{< /note >}}

{{< note >}}

  The AWS deployment should be configured with S3, Both Primary and New cluster should be configured with same s3 bucket.
  
{{< /note >}}

### Steps to set up the AMI Upgraded Cluster

1. Deploy the New cluster into a same/different region with S3 backup configuration.you can refer [AWS Deployment steps](/automate/ha_aws_deploy_steps/#deployment).

2. Do the backup configuration only when you have not provided the (backup information) configuration at the time of deployment. Refer backup section for [s3 configuration](/automate/ha_backup_restore_aws_s3/#configuration-in-provision-host).

3. On Primary Cluster

    - Take a backup of Primary cluster from bastion by running below command:

    ```sh
    chef-automate backup create --no-progress > /var/log/automate-backups.log
    ```

    - Create a bootstrap bundle, this bundle captures any local credentials or secrets that aren't persisted in the database. To create the bootstrap bundle, run the following command in one of the Automate nodes:

    ```sh
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - Copy `bootstrap.abb` to all Automate and Chef Infra frontend nodes in the New cluster.


4. On New AMI upgraded Cluster

    - Install `bootstrap.abb` on all the Frontend nodes (Chef-server and Automate nodes) by running the following command:

    ```sh
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
    ```

    - Run the following command in bastion to get the ID of the backups:

    ```sh
    chef-automate backup list
    ```

    - Make sure all the services in New cluster are up and running by running the following command from bastion:

    ```sh
    chef-automate status
    ```

    - On New Cluster Trigger restore command from bastion. 

        - For Chef Managed OpenSearch follow the below steps:

        ```bash
        sudo chef-automate config show > current_config.toml
        ```

        - Add the below config into `current_config.toml` (without any changes) and copy `current_config.toml` to bastion

        ```bash
        [global.v1.external.opensearch.auth.basic_auth]
            username = "admin"
            password = "admin"
        ```

        - On New cluster, use the following restore command to restore the backup of Primary Cluster from bastion.

        ```sh
        sudo chef-automate backup restore s3://<s3-bucket-name>/<path-to-backup>/<backup-id>/ --patch-config /path/to/current_config.toml --airgap-bundle /path/to/airgap-bundle --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"

        ```

- If you want to reuse the same custom domain used previously, update your DNS record to point to the Load-Balancer of the new cluster.

- Once the restore is successful you can destroy the Primary Cluster.
