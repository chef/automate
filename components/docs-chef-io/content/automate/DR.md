+++
title = "Disaster Recovery"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Disaster Recovery"
    parent = "automate/deploy_high_availability/DR SetUp"
    identifier = "automate/deploy_high_availability/dr.md Disaster Recovery"
    weight = 220 +++
+++

## How To SetUp Disaster Recovery Cluster For OnPrem Deployment

If the Frequency of Data Sync between the Live System(Production cluster) and DR could be in the range of 6 to 24 hrs, then we have 2 option's which utilize the regular backup and restore cadence, that syncs data from the Production cluster to the DR cluster. Typically these two clusters are located in different data centres or cloud provider regions.

### SetUp DR With Acceptable Down time for few hours

In this approach we have a cluster running, which perform the `chef-automate backup` operation at regular interval. In case of failure scenario  we have to setup a new cluster (fresh [deployment](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine)) and do the `chef-automate restore` operation in on the new cluster.

#### Cavet with Above approach

- Setup the new cluster will take couple of hours of time.
- Restore operation will take good amount of time, based on the size of backed up data.
- Chef-Automate HA cluster will not be available, hence no ingestion of data from any nodes.

{{< note >}}

- After the Deployment on Primary cluster, create bootstrap bundle and save it. Use below command to create the bundle. Run the below command to one of the Automate node.

```sh
chef-automate bootstrap bundle create bootstrap.abb
```

- After Installation DR Cluster, copy the bootstrap bundle to all the Frontend node and install with below command

```sh
chef-automate bootstrap bundle unpack bootstrap.abb
```

{{< /note >}}

#### Steps to StepUp DR Cluster

1. Please follow the fresh [deployment](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine).

2. Configure the backup at Automate HA cluster, in case if you have not configured, please refer this [Doc: Pre Backup Configuration for File System Backup](/automate/ha_backup_restore_prerequisites/#pre-backup-configuration-for-file-system-backup)

3. From the Step 2, you will get the backup mount path.

4. Stop all the services at frontend nodes in Automate HA Cluster.

   - Run the below command to all the Automate and Chef Infra Server nodes

    ``` bash
    sudo chef-automate stop
    ```

5. To run the restore command we need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`.
    - In case of airgap bundle is not present at `/var/tmp`, in that case we can copy the bundle from the bastion node to the Automate node. 

6. Run the command at the Chef-Automate node of Automate HA cluster to get the applied config

    ```bash
    sudo chef-automate config show > current_config.toml 
    ```

7. Add the OpenSearch credentials to the applied config.

    - If using Chef Managed Opensearch, then add the below config into `current_config.toml` (without any changes).

        ```bash
        [global.v1.external.opensearch.auth.basic_auth]
            username = "admin"
            password = "admin"
        ```

8. To restore the A2HA backup on Chef Automate HA, run the following command from any Chef Automate instance of the Chef Automate HA cluster:

    ```cmd
    sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
    ```

9. After the restore is successfully executed, you will see the below message:
  
    ```bash
    Success: Restored backup 20210622065515
    ```

10. Copy the `bootstrap.abb` bundle to all the Frontend nodes of the Chef Automate HA cluster. Unpack the bundle using the below command on all the Frontend nodes.

    ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
    ```

11. Start the Service in all the frontend nodes with the below command.

    ``` bash
    sudo chef-automate start
    ```

### SetUp DR With Acceptable Down Time will be few minutes

![DR SetUp with 2 Parallel Cluster](/images/automate/DR-2-cluster.png)

In this approach we have to running 2 Parallel Cluster of same capacity.

- Primary Cluster
- DR Cluster

Primary cluster will be in use and taking the backup at regular interval with `chef-automate backup` command. At the same time we will be restoring the backup at DR cluster via `chef-automate restore` command. In case of Failure, we will be change the DNS entry, DNS will point to the Load balancer of DR cluster.

#### Cavet with Above approach

- Setup two parallel cluster will be expensive.
- Data available till last backup performed.

