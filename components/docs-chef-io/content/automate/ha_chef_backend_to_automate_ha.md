+++
title = "Chef Backend to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Backend to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_chef_backend_to_automate_ha.md Chef Backend to Automate HA"
    weight = 210
+++

{{< warning >}}
Customers who are using only **Chef Backend** are advised to follow this migration guidance. Customers using **Chef Manage** or **Private Chef Supermarket** with Chef Backend should not migrate now.
{{< /warning >}}

This page explains the procedure to migrate the existing Chef infrastructure data to the newly deployed Chef Automate HA. This migration involves two steps:

-   Back up the data from an existing network infrastructure (chef-server) that has Chef Automate installed.
-   Restore the backed-up data to the newly deployed Chef Automate HA environment (chef-server).

There can be two possible scenarios:

-   Migrating from standalone Chef Server to Chef Automate Chef Server, part of a2-ha-backend frontend nodes cluster.

-   Migrating from Chef Backend cluster to Chef Automate Chef Server, part of a2-ha-backend frontend nodes cluster

Take backup using the _knife-ec-backup_ utility and move the backup folder to the newly deployed Chef Server. Later, restore using the same utility. The backup migrates all the cookbooks, users, data-bags, policies, and organizations.

`knife-ec-backup` utility backups and restores the data in an Enterprise Chef Server installation, preserving the data in an intermediate, editable text format. It is similar to the knife download and knife upload commands and uses the same underlying libraries. It includes workarounds for unsupported objects by the tools and various Server API deficiencies. The goal is to improve knife download, knife upload, and the Chef Infra Server API to deprecate the tool.

{{< note >}}
- The migration procedure is tested on Chef Server version 14+.
- The migration procedure is possible above Chef Backend version 2.1.0.
{{< /note >}}

## Backup the Existing Chef Server Data

-   Execute the `curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh \ | sudo bash` command to install Habitat.

-   Execute the `hab pkg install chef/knife-ec-backup` command to install the habitat package for _knife-ec-backup_.

-   Execute the `hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s <chef server URL> -u <pivotal> -k <path of pivotal>` command to generate a knife tidy server report to examine the stale nodes and new cookbooks. In this command:
    -   `pivotal` is the name of the user
    -   `path of pivotal` is the path where the user's pem file is stored.
    -   `node-threshold NUM_DAYS` is the maximum number of days since the last checking before a node is considered stale.

For example, `hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s https://chef.io -u pivotal -k /etc/opscode/pivotal.pem`.

-   Execute the `hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s <chef server URL>` command to initiate a backup of your Chef Server data. In this command:
    -   `with-user-sql` is required to handle user passwords and ensure user-specific association groups that are not duplicate.
    -   `--with-key-sql` is to handle cases where customers have users with multiple pem keys associated with their user or clients. The current chef-server API only dumps the default key. Sometimes, users will generate and assign additional keys to give additional users access to an account but still be able to lock them out later without removing everyone's access.

For example, `hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s https://chef.io`.

-   Execute the `hab pkg exec chef/knife-ec-backup knife tidy server clean --backup-path /path/to/an-ec-backup` command to clean unused data from reports. _Optional_

-   Execute the `scp -i /path/to/key backup\_$(date '+%Y%m%d%H%M%s') user@host:/home/user` command to copy the backup directory to the Chef Server frontend.

## Restore Backed Up Data to Chef Automate HA

-   Execute the `hab pkg install chef/knife-ec-backup` command to install the habitat package for _knife-ec-backup_.

-   Execute the `hab pkg exec chef/knife-ec-backup knife ec restore /home/centos/backup\_2021061013191623331154 -yes --concurrency 1 --webui-key /hab/svc/automate-cs-oc-erchef/data/webui\_priv.pem --purge -c /hab/pkgs/chef/chef-server-ctl/*/*/omnibus-ctl/spec/fixtures/pivotal.rb` command to restore the backup.

## In place migration (Chef Backend to Automate HA)

As part of this scenario, customer will migrate from chef-backend (5 machines) to Automate HA in-place, i.e. Automate HA will be deployed in those 5 machines only where Chef-backend is running. One extra bastion node will be required which will be managing the deployment of Automate HA on the chef backend infrastructure.



{{< note >}}
This will require downtime, so plan accordingly. A reduced performance should be expected with this. 
{{< /note >}}

{{< note >}}
- Need to setup your workstation based on newly created Autoamte-HA's chef-server. Only needed if you have setup the workstation earlier. 
- This inplace migration works only when cookbook are stored at database. This do not support use-case, where cookbooks are stored at filesystem. 
{{< /note >}}

- ssh to all the backend nodes of chef-backend and run `$chef-backend-ctl stop`
- ssh to all frontend nodes of chef-backend and run `$chef-server-ctl stop`
- Create one bastion machine in same vpc as chef-infra-server is in.
- Login to bastion machine and download chef-automate cli. https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip
- Extract downloaded zip file
- Create a airgap bundle using command `$ ./chef-automate airgap bundle create `
- Geneate config.toml file using `$ ./chef-automate init-config-ha existing_infra `
- Edit config.toml and your ips and fqdn, and update your keys. Make sure to provide Chef backend's frontend server IPs for Automate HA Chef Automate and Chef Server. Make sure to provide Chef backend's backend server IPs for Automate HA Postgres and OpenSearch machines.
- Deploy using `$ ./chef-automate deploy config.toml <airgapped bundle name>
- Clean up the old packages from chef-backend (like Elasticsearch and postgres)