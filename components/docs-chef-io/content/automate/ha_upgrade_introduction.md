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
{{% automate/ha-warn %}}
{{< /warning >}}

Steps to upgrade the Chef Automate HA are as shown below:

- Download the latest cli 
  ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

- Download Airgapped Bundle, download latest Bundle with this:

  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
  ```
  Download specific version bundle with this:
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

- If we want to only upgrade BackEnd Services i.e. Postgresql and OpenSearch.
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends
  ```

- To upgrade full Chef Automate HA System run this command from Bation Host: 
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib
  ```


{{< note >}}

  - BackEnd upgrades will restart the backend service, which take time for cluster to be in health state.
  - Upgrade command, currently only supports minor upgrade.  
{{< /note >}}

- To skip user confirmation prompt in upgrade, you can pass a flag
  ```bash 
    chef-automate upgrade run --airgap-bundle latest.aib --auto-approve
    OR 
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends --auto-approve
    OR
    chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends --auto-approve
  ```

Upgrade will also check for new version of bastion workspace, if new version is available, it will promt for a confirmation for workspace upgrade before upgrading the Frontend or backend nodes, 

In case of yes, it will do workspace upgrade and no will skip this.
We can also pass a flag in upgade command to avoid prompt for workspace upgrade. 

  ```bash
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade yes
      OR  
   chef-automate upgrade run --airgap-bundle latest.aib --auto-approve --workspace-upgrade no
  ```


