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

If the Frequency of Data Sync between the Live System(Production cluster) and DR could be in the range of 6 to 24 hrs, then we have 2 option's which utilize the regular backup and restore cadence, that syncs the data from the Production cluster to the DR cluster. Typically these two clusters are located in different data centres or cloud provider regions.

### Acceptable Down time for few hours

In this approach we have a cluster running, which perform the `chef-automate backup` operation at regular interval. In case of failure scenario  we have to setup a new cluster (fresh [deployment](/automate/ha_onprim_deployment_procedure/#Run-these-steps-on-Bastion-Host-Machine)) and do the `chef-automate restore` operation in on the new cluster.

#### Cavet with Above approach

- Setup the new cluster will take couple of hours of time.
- Restore operation will take good amount of time, based on the size of backed up data.
- Chef-Automate HA cluster will not be available, hence no ingestion of data from any nodes.

### Acceptable Down Time will be few minutes

![DR SetUp with 2 Parallel Cluster](/images/automate/DR-2-cluster.png)

In this approach we have to running 2 Parallel Cluster of same capacity.

- Primary Cluster
- DR Cluster

Primary cluster will be in use and taking the backup at regular interval with `chef-automate backup` command. At the same time we will be restoring the backup at DR cluster via `chef-automate restore` command. In case of Failure, we will be change the DNS entry, DNS will point to the Load balancer of DR cluster.

#### Cavet with Above approach

- Setup two parallel cluster will be expensive.
- Data available till last backup performed.

#### How to