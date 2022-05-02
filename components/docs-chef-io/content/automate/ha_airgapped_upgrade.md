+++
title = "Airgapped Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Airgapped Upgrade"
    parent = "automate/deploy_high_availability/ha_upgrade"
    identifier = "automate/deploy_high_availability/ha_upgrade/ha_airgapped_upgrade.md Airgapped Upgrade"
    weight = 220
+++

This page lists the commands that aid in upgrading the Chef Automate High Avainability (HA). In Chef Automate High Avainability (HA), upgrades are allowed only for frontend nodes. THe backend node (PostgreSQL, OpenSearch) upgrades are not allowed using any command.

Login to the Automate instance and check the current version of Chef Automate using the following command:

```sh
chef-automate version
```

The above command gives the automate and CLI version. Using the automate version, check the current automate from [Chef Automate Release Notes](https://docs.chef.io/release_notes_automate/).

Upgrade is possible if the current version is less that the latest version. To upgrade frontend with airgapped bundle, login to the machine with internet and follow the steps given below:

- Download **chef-automate cli** using the command below:

```sh
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

The above command gives the latest available automate CLI.

- Run the `./chef-automate airgap bundle create` to get the latest available automate bundle.

- Copy the newly downloaded **airgap bundle** and **chef-automate cli** on the non-internet environment using  `scp -i your-private-key.pem airgap-bundle.aib user@destination-ip-addess-172-32-0-1:airgap-bundle.aib scp -i your-private-key.pem chef-automate user@destination-ip-addess-172-32-0-1:chef-automate` command.

- Login to the non-internet bastion host and run the `chef-automate upgrade run --airgap-bundle </path/to/arigap-bundle>` command. The command will trigger the upgrade and will also receive the logs. Once the upgrade is complete, you we will receive an output with all the *Automate HA* instance details.

- Login to the Automate instance and run the version command: `chef-automate version`. The output will provide the upgraded version details which is the [latest version](https://docs.chef.io/release_notes_automate/).
