+++
title = "Disaster Recovery"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Disaster Recovery"
    parent = "automate/deploy_high_availability/DR SetUp"
    identifier = "automate/deploy_high_availability/dr.md Disaster Recovery"
    weight = 220
+++

## How To SetUp Disaster Recovery Cluster For OnPrem Deployment

If the Frequency of Data Sync between the Live System(Production cluster) and DR could be in the range of 6 to 24 hrs, then we have a option's which utilize the regular backup and restore cadence, that syncs data from the Production cluster to the DR cluster. Typically these two clusters are located in different data centres or cloud provider regions.

### Setup DR With Acceptable Down Time will be few minutes

![DR SetUp with 2 Parallel Cluster](/images/automate/DR-2-cluster.png)

In this approach we have to running 2 Parallel Cluster of same capacity.

- Primary Cluster
- DR Cluster

Primary cluster will be in use and taking the backup at regular interval with `chef-automate backup` command. At the same time we will be restoring the backup at DR cluster via `chef-automate restore` command.
In case of Primary Cluster Failure, we will be change the DNS entry.

#### Caveat with Above approach

- Setup two parallel cluster will be expensive.
- Data available till last backup performed.

#### Steps to Setup the Production and DR Cluster

1. Please follow the steps for fresh [deployment](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine) for Production cluster.

2. DR Cluster will be same as the Production cluster, we can use the above steps to setup the DR cluster.

3. Do the backup configuration as explained in backup section for [file system](/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-file-system-backup) or [object storage](https://deploy-preview-7425--chef-automate.netlify.app/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-object-storage).

4. On Production Cluster
    - One of the Chef Automate node, We configure the cron which trigger the `chef-automate backup` at certain interval.

        For example : sample cron look like for backup.

    ```sh
        sudo chef-automate backup create
    ```

    - Create bootstrap bundle and save it. Use below command to create the bootstrap bundle. Run the below command to one of the Automate node.

    ```sh
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - Copy `bootstrap.abb` to all the of Frontend nodes of DR Cluster.

    {{< note >}}
    - Suggested frequecy of backup cron to restore cron is 2:1 i.e., for every two successful backups in production cluster there should be one restore in DR cluster.
    - Always restore the latest backed-up data
    - 'Cron Job' - A cron job is a Linux command used to schedule a job that is executed periodically
    {{< /note >}}

5. On DR Cluster
    - Install `bootstrap.abb` on all the Frontend nodes, as explain in below example

        ```cmd
            sudo chef-automate bootstrap bundle unpack bootstrap.abb
        ```

    - We do not recommend to trigger the backup from the DR Cluster, unless it is using as a primary cluster.

    - Stop all the services on all the Frontend node, as explain  below command.

        ```sh
            systemctl stop chef-automate
        ```

    - Make sure that both backup and restore cron are align.

    - Configure the cron which trigger  the `chef-automate backup restore`  on one of the Chef Automate node.

        - To run the restore command we need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`.

        - In case of airgap bundle is not present at `/var/tmp`, in that case we can copy the bundle from the bastion node to the Automate node.

        - Run the command at the Chef-Automate node of Automate HA cluster to get the applied config

            ```bash
            sudo chef-automate config show > current_config.toml 
            ```

        - Add the OpenSearch credentials to the applied config.

            - If using Chef Managed Opensearch, then add the below config into `current_config.toml` (without any changes).

                ```bash
                [global.v1.external.opensearch.auth.basic_auth]
                    username = "admin"
                    password = "admin"
                ```

        - To restore backup on Chef Automate HA, below is the restore command, which can be trigger from any Chef Automate instance of DR cluster. Below restore command is an example command for the file system as a backup options.

            For example : sample cron look like for restore for file system.

            ```cmd
                id=$(sudo chef-automate backup list | tail -1 | awk '{print $1}')
                sudo chef-automate backup restore /mnt/automate_backups/backups/$id/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
            ```

#### Steps to Switch to DR Cluster

- Stop the Restore cron

- Start all the service on all the Frontend node.

- Update the DNS entry. Now DNS will point to the DR Load balancer.

- After doing the above steps, DR Cluster will be primary cluster.

- Need to setup the backup cron, so that it will perform the backup.