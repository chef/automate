+++
title = "Disaster Recovery Setup"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Disaster Recovery for AWS Deployment"
    parent = "automate/deploy_high_availability/disaster_recovery"
    identifier = "automate/deploy_high_availability/ha_disaster_recovery_setup_AWS.md Disaster Recovery"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

## Setup Disaster Recovery Cluster For AWS Deployment

Recovery Point Objective (RPO) is the maximum acceptable amount of time since the last data recovery point, if an RPO of 1 to 24 hours is acceptable then using a typical backup and restore strategy for your disaster recovery plan is recommended.
Typically these two clusters should be located in different data centers or cloud provider regions.

### Requirements

1. Two identical clusters located in different AWS regions.
1. Amazon S3 access in both regions for Application backup.
1. Ability to schedule jobs to run backup and restore commands in both clusters. We recommend using corn or a similar tool like anacron.

In the above approach, there will be two identical clusters

- Primary Cluster (or Production Cluster)
- Disaster Recovery Cluster

![Disaster Recovery Setup with 2 Identical Clusters](/images/automate/DR_AWS_Deployment.png)

The primary cluster will be active and regular backups will be performed using `chef-automate backup create`. At the same time, the disaster recovery cluster will be restoring the latest backup data using `chef-automate backup restore`.

When the primary cluster fails, accomplish the failover by updating DNS records to the DR cluster Load balancer.

### Caveat with the above approach

- Running two parallel clusters can be expensive.
- The amount of data loss will depend on how frequently backups are performed in the Primary cluster.
- Changing DNS records from the Primary load-balancer to the Disaster Recovery load-balancer can take time to propagate through the network.

### Steps to set up the Production and Disaster Recovery Cluster

1. Deploy the Primary cluster following the deployment instructions by [clicking here](/automate/ha_aws_deploy_steps/#deployment).

1. Deploy the Disaster Recovery cluster into a different data center/region using the same steps as the Primary cluster

1. Do the backup configuration only when you have not provided the (backup information) configuration at the time of deployment. Refer backup section for [object storage](/automate/ha_backup_restore_aws_s3/).

    {{< note >}}
    - During the deployment for the Primary and DR clusters, use the same S3 bucket name.
    - Configure backups for both clusters using only [object storage](/automate/ha_backup_restore_aws_s3/).
    {{< /note >}}

1. On Primary Cluster

    - Configure a cronjob to regularly run the `chef-automate backup` command from one of the Chef Automate nodes. The sample cron for backup looks like this:

    ```sh
    chef-automate backup create --no-progress > /var/log/automate-backups.log
    ```

    - Create a bootstrap bundle; this bundle captures any local credentials or secrets that aren't persisted to the database. To create the bootstrap bundle, run the following command in one of the Automate nodes:

    ```sh
    chef-automate bootstrap bundle create bootstrap.abb
    ```

    - Copy `bootstrap.abb` to all Automate and Chef Infra frontend nodes in the Disaster Recovery cluster.

    {{< note >}}
    - Suggested frequency of backup and restore jobs is one hour. Monitor backup times to ensure they can be completed in the available time.
    - Make sure the Restore cron always restores the latest backed-up data.
    - A cron job is a Linux command used to schedule a job that is executed periodically.
    {{< /note >}}

    - To clean the data from the backed-up storage, either schedule a cron or delete it manually.
        - To prune all but a certain number of the most recent backups manually, parse the chef-automate backup list's output and apply the chef-automate backup delete command.
        For example:

        ```sh
        export KEEP=10; export HAB_LICENSE=accept-no-persist; chef-automate backup list --result-json backup.json > /dev/null && hab pkg exec core/jq-static jq "[.result.backups[].id] | sort | reverse | .[]" -rM backup.json | tail -n +$(($KEEP+1)) | xargs -L1 -i chef-automate backup delete --yes {}
        ```

1. On Disaster Recovery Cluster

    - Install `bootstrap.abb` on all the Frontend nodes (Chef-server and Automate nodes) by running the following command:

    ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
    ```

    - We don't recommend creating backups from the Disaster Recovery cluster unless it has become the active cluster and receiving traffic from the clients/nodes.

    - Stop all the services on all Automate and Chef Infra frontend nodes using the following command:

    ```sh
    systemctl stop chef-automate
    ```

    - Make sure both backup and restore cron are aligned.

    - Run the following command in one of the Automate nodes to get the IDs of all the backups:

    ```sh
    chef-automate backup list
    ```

    - Configure the cron that triggers the `chef-automate backup restore` on one of the Chef Automate nodes.

        - To run the restore command, you need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. For example: `frontend-4.x.y.aib`.

        - In case the airgap bundle is not present at `/var/tmp`, it can be copied from the bastion node to the Automate frontend node

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

        - In the Disaster Recovery cluster, use the following sample command to restore the latest backup from any Chef Automate frontend instance.

        ```cmd
        id=$(chef-automate backup list | grep completed | tail -1 | awk '{print $1}')
        sudo chef-automate backup restore <backup-url-to-object-storage>/automate/$id/ --patch-config /path/to/current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
        ```

If the restore is unsuccessful check [Troubleshooting](/automate/ha_backup_restore_aws_s3/#troubleshooting)
### Switch to Disaster Recovery Cluster

Steps to switch to the disaster recovery cluster are as follows:

- Stop the backup restore cron.
- Start the services on all the Automate and Chef Infra frontend nodes, using the below command:

    ```sh
    systemctl start chef-automate
    ```

- Update the Automate FQDN DNS entry to resolve the Disaster Recovery load balancer.
- The Disaster Recovery cluster will be the primary cluster. It may take some time for DNS changes to propagate fully.
- Set up a backup cron to start taking backups of the active cluster.
