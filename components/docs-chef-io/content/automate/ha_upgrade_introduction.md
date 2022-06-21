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
Upgrade will also check for new version of bashtion workspace, if new version is available, it will promt for a confirmation for workspace upgrade before upgrading, 

In case of yes, it will do workspace upgrade and no will skip this.

you can also pass a flag in upgade command to avoid promt. 

``` 
--workspace-upgrade yes/no 
```

Steps to upgrade the Chef Automate HA are as shown below:

- Download Airgapped Bundle \
  Download latest Bundle with this:

  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
  ```
  Download specific version bundle with this, example version: 4.0.91:
  ```bash
  curl https://packages.chef.io/airgap_bundle/current/automate/4.0.91.aib -o automate-4.0.91.aib
  ```
- If we want to only upgrade FrontEnd Services i.e. Chef Automate and Chef Infra Server.
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-frontends
  ```
- If we want to only upgrade BackEnd Services i.e. Postgresql and OpenSearch.
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib --upgrade-backends
  ```
  BackEnd upgrades take time to restart.
- To upgrade full Chef Automate HA System run this command from Bation Host: 
  ```bash
  chef-automate upgrade run --airgap-bundle latest.aib
  ```
  BackEnd upgrades take time to restart.

  to skip user confirmation promt in upgrade pass a flag
  ```
  --auto-approve
  ```


