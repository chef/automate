+++
title = "Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Upgrade"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_upgrade_introduction.md Upgrade HA"
    weight = 70
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

Steps to upgrade the Chef Automate HA are as shown below:

- Download the latest CLI using:

  ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

- Download the latest airgapped Bundle by running the following command:

  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
  ```

  Download a specific version bundle with this, example version: 4.0.91:

  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/<version>.aib -o automate-<version>.aib
  ```

  {{< note >}}
  Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
  {{< /note >}}

- If we want to only upgrade FrontEnd Services i.e. Chef Automate and Chef Infra Server.

  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends
  ```

- If you only want to upgrade BackEnd Services i.e., Postgresql and OpenSearch, run the following command:

  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends
  ```

- To upgrade the entire Chef Automate HA System, run the following command from Bastion Host:

  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib
  ```

{{< note >}}

- BackEnd upgrades will restart the backend service, which takes time for the cluster to be healthy.
- Upgrade command currently only supports minor upgrades.

{{< /note >}}

- To skip the user confirmation prompt for an upgrade, you can pass a flag using the following command:

  ```bash
    chef-automate upgrade run --airgap-bundle latest.aib --auto-approve
    OR 
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends --auto-approve
    OR
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends --auto-approve
  ```

Upgrade will also check for the new version of the bastion workspace. If a new version is available, it will prompt confirmation for workspace upgrade before upgrading the Frontend or backend nodes.

If you select **Yes**, it will upgrade the workspace; else, skip this step. We can also pass a flag in the upgrade command to avoid a prompt for a workspace upgrade.

  ```bash
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade yes
      OR  
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade no
  ```
