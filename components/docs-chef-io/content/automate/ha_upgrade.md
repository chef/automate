+++
title = "Upgrade Procedure"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Upgrade Procedure"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_upgrade.md Upgrade Procedure"
    weight = 80
+++

This page lists the commands that aid in upgrading the Chef Automate High Availability (HA) frontend and backend clusters in your network infrastructure.

{{< note >}}

- The upgrade of the backend nodes restarts ElasticSearch and PostgreSQL services.
- You cannot downgrade the Automate version. If you intend to install the lower version of the Chef Automate High Availability (HA) system, uninstall the existing version of the same, and destroy the terraform.

{{< /note >}}

## Command List

1. Execute the `chef-automate upgrade run --upgrade-airgap-bundles` command to upgrade both the bundles and deploy it on their respective nodes.

1. Execute the `chef-automate upgrade run --upgrade-airgap-bundles --skip-deploy` command to update the bundles. The *--skip-deploy* flag skips the deployment of these bundles.

1. Execute the `chef-automate upgrade run --upgrade-frontends` command to upgrade the frontend bundles and deploy it on the frontend clusters.

1. Execute the `chef-automate upgrade run --upgrade-frontends --skip-deploy` command to upgrade the frontend bundles. The *--skip-deploy* flag skips the deployment of the frontend bundles on their respective frontend nodes.

1. Execute the `chef-automate upgrade run --upgrade-backends` command to upgrade the backend bundles and deploy it on the backend clusters.

1. Execute the`chef-automate upgrade run --upgrade-backends --skip-deploy` command to upgrade the backend bundles. The *--skip-deploy* flag skips the deployment of the backend bundles on their respective backend nodes.
