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

If the Frequency of Data Sync between the Live System(Production cluster) and DR ranges between few hrs, then we have an option which utilize the regular backup and restore cadence, that syncs data from the Production cluster to the DR cluster.
Typically these two clusters should be located in different data centres or cloud provider regions.

In this approach we have to running 2 Parallel Cluster of same capacity.

- Primary Cluster (or Production Cluster)
- DR Cluster

![DR SetUp with 2 Parallel Cluster](/images/automate/DR-2-cluster.png)

Primary cluster will be in use and will take the backup at regular interval with `chef-automate backup create` command. At the same time DR cluster will be restoring the latest backup data via `chef-automate backup restore` command.
In case of Primary Cluster Failure, we can change the DNS routing to DR Cluster.

### Caveat with above approach

- Running two parallel cluster can be expensive.
- Data available till last backup performed.

### Steps to setup the Production and DR Cluster

1. Please follow the steps for fresh [deployment](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine) for Production cluster.

1. DR Cluster will be same as the Production cluster, we can use the above steps to setup the DR cluster.

1. Do the backup configuration as explained in backup section for [file system](/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-file-system-backup) or [object storage](https://deploy-preview-7425--chef-automate.netlify.app/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-object-storage).

    {{< note >}}

    Both the clusters  should be configured with same object storage or file system i.e.
    if Primary cluster is configured with a object storage or file system,
    then DR cluster should also configured with same object storage or file system.

    {{< /note >}}

1. On Primary Cluster

    - In one of the Chef Automate node, configure the cron which triggers the `chef-automate backup` command at certain interval.

        For example : sample cron for backup look like

        ```sh
        sudo chef-automate backup create
        ```

    - Create bootstrap bundle and save it some where save (or save it in DR cluster). To create the bootstrap bundle, run the following command in one of the Automate nodes.

        ```sh
        chef-automate bootstrap bundle create bootstrap.abb
        ```

    - Copy `bootstrap.abb` to all the of Frontend nodes of DR Cluster.

    {{< note >}}

    - Suggested frequency of backup cron and restore cron is one hour i.e., backup and restore in respective machines can be done as frequent as 1 hour
    - Make sure the Restore cron always restores the latest backed-up data
    - A cron job is a Linux command used to schedule a job that is executed periodically

    {{< /note >}}

1. On DR Cluster

    - Install `bootstrap.abb` on all the Frontend nodes (Chef-server and Automate nodes), by running the following command

        ```cmd
        sudo chef-automate bootstrap bundle unpack bootstrap.abb
        ```

    - We do not recommend to trigger the backup from the DR Cluster, unless it is switched as a primary cluster.

    - Stop all the services on all the Frontend node with the following command.

        ```sh
        systemctl stop chef-automate
        ```

    - Make sure that both backup and restore cron are align.

    - Run the following command in one of the Automate nodes to get the IDs of all the backup

        ```sh
        chef-automate backup list
        ```

    - Configure the cron which trigger the `chef-automate backup restore` on one of the Chef Automate node.

        - To run the restore command we need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`.

        - In case of airgap bundle is not present at `/var/tmp`, the same can be copied from the bastion node to the Automate node.

        - Run the command at the Automate node of Automate HA cluster to get the applied config

        ```bash
        sudo chef-automate config show > current_config.toml
        ```

        - Add the OpenSearch credentials to the applied config. If using Chef Managed Opensearch, then add the below config into `current_config.toml` (without any changes).

        ```bash
        [global.v1.external.opensearch.auth.basic_auth]
            username = "admin"
            password = "admin"
        ```

        - To restore backup on Chef Automate HA, below is the restore command, which can be trigger from any Chef Automate instance of DR cluster. Below restore command is an example command for the file system as a backup options.

            For example : Sample cron for restoring backup saved in file system look like

            ```cmd
            id=$(sudo chef-automate backup list | tail -1 | awk '{print $1}')
            sudo chef-automate backup restore /mnt/automate_backups/backups/$id/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
            ```

            Sample cron for restoring backup saved in object storage (S3) look like

            ```cmd
            id=$(sudo chef-automate backup list | tail -1 | awk '{print $1}')
            sudo chef-automate backup restore <backup-url-to-object-storage>/automate/$id/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
            ```

### Steps to switch to DR Cluster

- Stop the Restore cron
- Start all the service on all the Frontend node.
- Update the DNS entry. Now DNS will point to the DR Load balancer.
- After doing the above steps, DR Cluster will be primary cluster.
- Need to setup the backup cron, so that it will perform the backup.
