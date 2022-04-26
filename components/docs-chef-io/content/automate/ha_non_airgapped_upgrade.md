+++
title = "Non-Airgapped Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Non-Airgapped Upgrade"
    parent = "automate/deploy_high_availability/ha_upgrade"
    identifier = "automate/deploy_high_availability/ha_upgrade/ha_non_airgapped_upgrade.md Non-Airgapped Upgrade"
    weight = 210
+++

This page lists the commands that aid in upgrading the Chef Automate High Availability (HA) frontend and backend clusters in your network infrastructure.

{{< note >}}

- The backend nodes' upgrade restarts **OpenSearch** and **PostgreSQL** services.

- You cannot downgrade the Automate version. If you intend to install the lower version of the Chef Automate High Availability (HA) system, uninstall the existing version of the same, and destroy the terraform.

{{< /note >}}

## Upgrade Procedure

- Execute the `chef-automate upgrade run --upgrade-airgap-bundles` command to upgrade the bundles and deploy them on the respective nodes.

- Execute the `chef-automate upgrade run --upgrade-airgap-bundles --skip-deploy` command to update the bundles. The *--skip-deploy* flag skips the deployment of these bundles.

- Execute the `chef-automate upgrade run --upgrade-frontends` command to upgrade the frontend bundles and deploy it on the frontend clusters.

- Execute the `chef-automate upgrade run --upgrade-frontends --skip-deploy` command to upgrade the frontend bundles. The *--skip-deploy* flag skips the deployment of the frontend bundles on their respective frontend nodes.

{{< figure src="/images/automate/ha_upgrade.png" alt="Upgrade">}}

- Execute the `chef-automate upgrade run --upgrade-backends` command to upgrade the backend bundles and deploy it on the backend clusters.

- Execute the `chef-automate upgrade run --upgrade-backends --skip-deploy` command to upgrade the backend bundles. The *--skip-deploy* flag skips the deployment of the backend bundles on their respective backend nodes.

{{< figure src="/images/automate/ha_upgrade_complete.png" alt="Upgrade Completion Message">}}
