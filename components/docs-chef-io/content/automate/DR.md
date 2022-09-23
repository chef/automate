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

Suppose the Frequency of Data Sync between the Live System(Production cluster) and DR ranges between a few hours. In that case, we have an option that utilizes the regular backup and restores cadence, which syncs data from the Production cluster to the disaster recovery cluster. Typically these two clusters should be located in different data centers or cloud provider regions.

In the above approach, you have to run 2 Parallel Clusters of the same capacity.

- Primary Cluster (or Production Cluster)
- Disaster Recovery Cluster

![DR SetUp with 2 Parallel Cluster](/images/automate/DR-2-cluster.png)

The primary cluster will be in use and take the backup regularly with the `chef-automate backup create` command. At the same time, the disaster recovery cluster will be restoring the latest backup data via the `chef-automate backup restore` command.
In case of Primary Cluster Failure, we can change the DNS routing to DR Cluster.

### Caveat with the above approach

- Running two parallel clusters can be expensive.
- Data is available till the last backup is performed.

### Steps to setup the Production and Disaster Recovery Cluster

1. Click [here](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine) to follow the steps for fresh deployment for Production cluster.

1. Disaster Recovery Cluster will be the same as the Production cluster; we can use the above steps to set up the diisaster recovery cluster.

1. Do the backup configuration as explained in backup section for [file system](/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-file-system-backup) or [object storage](https://deploy-preview-7425--chef-automate.netlify.app/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-object-storage).

    {{< note >}}
    Configure both the clusters with the same object storage or file system i.e. if the primary cluster is configured with object storage or file system, configure the disaster recovery cluster with the same object storage or file system.
    {{< /note >}}

1. On Primary Cluster

    - In one of the Chef Automate nodes, configure the cron which triggers the `chef-automate backup` command at a certain interval. The sample cron for backup looks like:

    ```sh
    sudo chef-automate backup create
    ```

    - Create a bootstrap bundle and save it somewhere (or save it in the disaster recovery cluster). To create the bootstrap bundle, run the following command in one of the Automate nodes:

    ```sh
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - Copy `bootstrap.abb` to all Frontend nodes of DR Cluster.

    {{< note >}}
    - Suggested frequency of backup cron and restore cron is one hour, i.e., backup and restore in respective machines can be done as frequent as 1 hour.
    - Make sure the Restore cron always restores the latest backed-up data.
    - A cron job is a Linux command used to schedule a job that is executed periodically.
    {{< /note >}}

1. On Disaster Recovery Cluster

    - Install `bootstrap.abb` on all the Frontend nodes (Chef-server and Automate nodes) by running the following command:

    ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
    ```

    - We do not recommend triggering the backup from the disaster recovery cluster unless it is switched as a primary cluster.

    - Stop all the services on all the frontend nodes using the following command:

    ```sh
    systemctl stop chef-automate
    ```

    - Make sure both backup and restore cron are aligned.

    - Run the following command in one of the Automate nodes to get the IDs of all the backup:

    ```sh
    chef-automate backup list
    ```

    - Configure the cron that triggers the `chef-automate backup restore` on one of the Chef Automate nodes.

        - To run the restore command, you need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. For example: `frontend-4.x.y.aib`.

        - In case the airgap bundle is not present at `/var/tmp`, the same can be copied from the bastion node to the Automate node.

        - Run the command at the Automate node of Automate HA cluster to get the applied config:

        ```bash
        sudo chef-automate config show > current_config.toml
        ```

        - Add the OpenSearch credentials to the applied config. If using Chef Managed OpenSearch, add the config below into `current_config.toml` (without any changes).

        ```bash
        [global.v1.external.opensearch.auth.basic_auth]
            username = "admin"
            password = "admin"
        ```

        - To restore backup on Chef Automate HA, below is the restore command, which can be triggered from any Chef Automate instance of the Disaster Recovery cluster. Below the restore command is an example command for the file system as a backup option. Sample cron for restoring backup saved in file system looks like:

        ```cmd
        id=$(sudo chef-automate backup list | tail -1 | awk '{print $1}')
        sudo chef-automate backup restore /mnt/automate_backups/backups/$id/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
        ```

        Sample cron for restoring backup saved in object storage (S3) looks like this:

        ```cmd
        id=$(sudo chef-automate backup list | tail -1 | awk '{print $1}')
        sudo chef-automate backup restore <backup-url-to-object-storage>/automate/$id/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
        ```

### Switch to Disaster Recovery Cluster

Steps to switch to the disaster recovery cluster are as follows:

- Stop the Restore cron.
- Start all the services on all the Frontend nodes.
- Update the DNS entry. Now DNS will point to the DR Load balancer.
- After the above steps, DR Cluster will be the primary cluster.
- Need to set up the backup cron so that it will perform the backup.
