+++
title = "Chef Backend to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Backend to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_chef_backend_to_automate_ha.md Chef Backend to Automate HA"
    weight = 220
+++

This page explains the procedure to migrate the existing Chef infrastructure data to the newly deployed Chef Automate HA. This migration involves two steps, which are:

- Backup the data from on existing network infrastrcure (chef-server) that has Chef Automate installed
- Restore the backed up data to the newly deployed Chef Automate HA environment (chef-server)

There can be possibly two scenarios,

- Migrating from standalone Chef Server to Chef Automate Chef Server, which is part of a2-ha-backend frontend nodes cluster

- Migrating from Chef Backend cluster to Chef Automate Chef Server, which is part of a2-ha-backend frontend nodes cluster

In both the cases we need to take backup using *knife-ec-backup* utility and then move the backup folder to the newly deployed Chef Server. Later, you need to take restore using the same utility. This backup migrates all the cookbooks, users, data-bags, policies, and organisations.

`knife-ec-backup` utility backups and restores the data in an Enterprise Chef Server installation, preserving the data in an intermediate, editable text format. It is similar to the knife download and knife upload commands and uses the same underlying libraries. It includes workarounds for objects not yet supported by those tools and various Server API deficiencies. The goal is to improve knife download, knife upload, and the Chef Infra Server API and deprecate this tool.

{{< note >}}

The migration procedure is tested on Chef Server version 14+.

{{< /note >}}

## Backup the existing Chef Server Data

1. Execute command `curl <https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh> | sudo bash` to install Habitat.

1. Execute command `hab pkg install chef/knife-ec-backup` to install the habitat package for *knife-ec-backup*.

1. Execute command `hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s <chef server URL> -u <pivotal> -k <path of pivotal>` to generate a knife tidy server report to examine the stale nodes and unused cookbooks.

where`pivotal` is the name of user, `path of pivotal` is the path where user's pem file is stored, and `node-threshold NUM_DAYS` is the maximum number of days since last checking before node is considered stale.

For example, `hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s https://chef.io -u pivotal -k /etc/opscode/pivotal.pem`.

1. Execute command `hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem --with-user-sql --with-key-sql -s <chef server URL>` to initiate a backup of your Chef Server data.

where,

`--with-user-sql` is required to handle user passwords and to ensure user specific association groups are not duplicated.

`--with-key-sql` is to handle cases where customers have users with multiple pem keys associated with their user or clients. The current chef-server API only dumps the default key and sometimes users will generate and assigned additional keys to give additional users access to an account but still be able to lock them out later without removing everyones access.

For example, `hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem --with-user-sql --with-key-sql -s https://chef.io`.

1. Execute command `hab pkg exec chef/knife-ec-backup knife tidy server clean --backup-path /path/to/an-ec-backup` to clean unused data from reports. _Optional_

1. Execute command `scp -i /path/to/key backup\_$(date '+%Y%m%d%H%M%s') user@host:/home/user` to copy the backup directory to the Chef Server frontend.

## Restore Backed Up Data to Chef Automate HA

1. Execute command `hab pkg install chef/knife-ec-backup` to install the habitat package for *knife-ec-backup*.

1. Execute command `hab pkg exec chef/knife-ec-backup knife ec restore /home/centos/backup\_2021061013191623331154 -yes --concurrency 1 --webui-key /hab/svc/automate-cs-oc-erchef/data/webui\_priv.pem --purge -c /hab/pkgs/chef/chef-server-ctl/*/*/omnibus-ctl/spec/fixtures/pivotal.rb` to restore the backup.
