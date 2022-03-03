+++
title = "Upgrade Chef Automate to 3.x.x"
date = 2022-03-03T12:02:46-08:00
draft = false

[menu]
  [menu.automate]
    title = "Upgrade to 3.x.x"
    identifier = "automate/major_upgrade.md Major Upgrade"
    parent = "automate"
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/major_upgrade.md)

By default, Chef Automate automatically upgrades to the latest version. Milestone versions dont automatically upgrade to the latest version. We use the `--major` flag to upgrade to the latest version if you are on a milestone version.

## Upgrade path for different user scenarios

The new major version of chef automate upgrades the embedded Postgres database from v9.6 to v13. Since this version changes the version and migrates user data, customers will need to perform various steps to take control of the system downtimes and plan their upgrade according to their needs.

There are 4 possible scenarios an user will be in during a major upgrade to 3.x.x version of chef automate. Please identify your automate environment and use the steps according to the scenario which fits you.

- [Customer running Automate with Embedded PG]({{< relref "major_upgrade.md#customer-running-automate-with-embedded-pg" >}}).
- [Customer running Automate with External PG]({{< relref "major_upgrade.md#customer-running-automate-with-external-pg" >}}).
- [Customer running Automate in Air Gapped Environment with Embedded PG]({{< relref "major_upgrade.md#customer-running-automate-in-air-gapped-environment-with-embedded-pg" >}}).
- [Customer running Automate in Air Gapped Environment with External PG]({{< relref "major_upgrade.md#customer-running-automate-in-air-gapped-environment-with-external-pg" >}})

### Customer running Automate with Embedded PG

To continue the major upgrade you need to make sure that automate is on the latest version (milestone version).
Make sure automate is up-to-date. Please run `chef-automate upgrade run` to upgrade automate.

Once upgraded to latest milestone version, you can now perform the major upgrade. Below are the steps for major upgrade:

- Make sure that new major version is available by running `chef-automate upgrade status`
- Run `chef-automate upgrade run --major` to start the upgrade process. You will be presented with upgrade information and displayed a checklist of requirements pre upgrade along with a list of steps to perform post upgrade. The steps are listed below :

    ```
    This is a Major upgrade.
    ========================

      1) In this release Embedded PostgreSQL is upgraded to version 13.5
      2) This will need special care if you use Embedded PostgreSQL.

    ===== Your installation is using Embedded PostgreSQL =====

      Please confirm this checklist that you have taken care of these steps
      before continuing with the Upgrade to version 3.0.0:

    You had planned for a downtime?: (y/n)
    y
    You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
    y
    Ensure you have more than 60 percent free disk space (y/n)
    y
    After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
    y

    Post Upgrade Steps:
    ===================

    1) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    2) Migrate Data from PG 9.6 to PG 13.5 using this command:
        $ chef-automate post-major-upgrade migrate --data=pg

    3) Check Automate UI everything is running and all data is visible

    4) If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running:
        $ chef-automate post-major-upgrade clear-data --data=PG

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

    Now, upgrade will start, Please confirm to continue... (y/n)
    ```
- Check the status of Upgrade: 
    ```
    $ chef-automate upgrade status 

    Automate is upgrading to airgap bundle (22.0.1) 

    $ chef-automate upgrade status 

    Automate is up-to-date with airgap bundle (22.0.1) 

    Post Upgrade Steps:
    ===================

    1) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    2) Migrate Data from PG 9.6 to PG 13.5 using this command:
        $ chef-automate post-major-upgrade migrate --data=pg

    3) Check Automate UI everything is running and all data is visible

    4) If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running:
        $ chef-automate post-major-upgrade clear-data --data=PG

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****
    ```
- Migrate your data from PostgreSQL v9.6 to v13 by running command:
  `chef-automate post-major-upgrade migrate --data=PG`
- Check all services are running: `chef-automate status`
- If you are sure all data is present in new Automate, then we can clear the old PostgreSQL data: `chef-automate post-major-upgrade clear-data --data=PG`
- Once all the above steps are complete, major version upgrade is successful.

### Customer running Automate with External PG

To continue the major upgrade you need to make sure that automate is on the latest version (milestone version).
Make sure automate is up-to-date. Please run `chef-automate upgrade run` to upgrade automate.

Once upgraded to latest milestone version, you can now perform the major upgrade. Below are the steps for major upgrade:

- Make sure that new major version is available by running `chef-automate upgrade status`
- Run `chef-automate upgrade run --major` to start the upgrade process. You will be presented with upgrade information and displayed a checklist of requirements pre upgrade along with a list of steps to perform post upgrade. The steps are listed below :

    ```
    This is a Major upgrade.
    ========================

      1) In this release Embedded PostgreSQL is upgraded to version 13.5
      2) This will need special care if you use Embedded PostgreSQL.

    ===== Your installation is using External PostgreSQL =====

      Please confirm this checklist that you have taken care of these steps
      before continuing with the Upgrade to version 3.0.0:

    You had planned for a downtime?: (y/n)
    y
    You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
    y
    Upgrade your PostgreSQL 9.6 to 13.5 with the help of your Database Administrator (y/n)
    y
    After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
    y

    Post Upgrade Steps:
    ===================

    1) If your PostgreSQL Connection URL and Credential are changed then update them by putting them in config.toml and patching it in using:
        $ chef-automate config patch config.toml

    2) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    3) Check all services are running using:
        $ chef-automate status

    4) Check Automate UI everything is running and all data is visible

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

    Now, upgrade will start, Please confirm to continue... (y/n)
    ```
- Check all services are running: `chef-automate status`
- Once all the above steps are complete, major version upgrade is successful.

### Customer running Automate in Air Gapped Environment with Embedded PG

To continue the major upgrade you need to check the release notes. And upgrade to the suitable milestone version based on the current automate version.
- First customer upgrades to latest milestone 20224354343234
- Next they perform the major upgrade to 3.x.x

Steps to upgrade to latest milestone version 20224354343234:
- Download Latest CLI using Internet Connected Machine: `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate `
- Create AIB bundle, on Internet Connected Machine: `chef-automate airgap bundle create /<path>/automate_v2022012400000.aib --version 20224354343234`
- Copy the CLI (chef-automate) and AIB (automate_v2022012400000.aib) to Air Gapped Machine where Chef Automate is Running.
- Run Upgrade, On Air Gapped Machine: `sudo chef-automate upgrade run --airgap-bundle /<path>/ automate_v2022012400000.aib`
- Check the status of Upgrade: 
    ```
    $ chef-automate upgrade status 

    Automate is upgrading to airgap bundle (2022012400000) 

    $ chef-automate upgrade status 

    Automate is up-to-date with airgap bundle (2022012400000) 
    ```
 

Once upgraded to latest milestone version, you can now perform the major upgrade. Below are the steps for major upgrade to 3.x.x:

- Create AIB Bundle on Internet Connected Machine: `chef-automate airgap bundle create /<path>/automate_v22.0.1.aib --version 3.x.x`
- Copy the CLI (chef-automate) and AIB (automate_v22.0.1.aib) to Air Gapped Machine where Chef Automate is Running. 
- Run Upgrade On Air Gapped Machine: `sudo chef-automate upgrade run --airgap-bundle /<path>/automate_v3.x.x.aib --major `. You will be presented with upgrade information and displayed a checklist of requirements pre upgrade along with a list of steps to perform post upgrade. The steps are listed below :
    ```
    This is a Major upgrade.
    ========================

      1) In this release Embedded PostgreSQL is upgraded to version 13.5
      2) This will need special care if you use Embedded PostgreSQL.

    ===== Your installation is using Embedded PostgreSQL =====

      Please confirm this checklist that you have taken care of these steps
      before continuing with the Upgrade to version 3.0.0:

    You had planned for a downtime?: (y/n)
    y
    You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
    y
    Ensure you have more than 60 percent free disk space (y/n)
    y
    After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
    y

    Post Upgrade Steps:
    ===================

    1) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    2) Migrate Data from PG 9.6 to PG 13.5 using this command:
        $ chef-automate post-major-upgrade migrate --data=pg

    3) Check Automate UI everything is running and all data is visible

    4) If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running:
        $ chef-automate post-major-upgrade clear-data --data=PG

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

    Now, upgrade will start, Please confirm to continue... (y/n)
    ```
- Check the status of Upgrade: 
    ```
    $ chef-automate upgrade status 

    Automate is upgrading to airgap bundle (22.0.1) 

    $ chef-automate upgrade status 

    Automate is up-to-date with airgap bundle (22.0.1) 

    Post Upgrade Steps:
    ===================

    1) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    2) Migrate Data from PG 9.6 to PG 13.5 using this command:
        $ chef-automate post-major-upgrade migrate --data=pg

    3) Check Automate UI everything is running and all data is visible

    4) If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running:
        $ chef-automate post-major-upgrade clear-data --data=PG

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****
    ```
- Migrate your data from PostgreSQL v9.6 to v13 by running command:
  `chef-automate post-major-upgrade migrate --data=PG`
- Check all services are running: `chef-automate status`
- If you are sure all data is present in new Automate, then we can clear the old PostgreSQL data: `chef-automate post-major-upgrade clear-data --data=PG`
- Once all the above steps are complete, major version upgrade is successful.

### Customer running Automate in Air Gapped Environment with External PG

To continue the major upgrade you need to check the release notes. And upgrade to the suitable milestone version based on the current automate version.
- First customer upgrades to latest milestone 20224354343234
- Next they perform the major upgrade to 3.x.x

Steps to upgrade to latest milestone version 20224354343234:
- Download Latest CLI using Internet Connected Machine: `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate `
- Create AIB bundle, on Internet Connected Machine: `chef-automate airgap bundle create /<path>/automate_v2022012400000.aib --version 20224354343234`
- Copy the CLI (chef-automate) and AIB (automate_v2022012400000.aib) to Air Gapped Machine where Chef Automate is Running.
- Run Upgrade, On Air Gapped Machine: `sudo chef-automate upgrade run --airgap-bundle /<path>/ automate_v2022012400000.aib`
- Check the status of Upgrade: 
    ```
    $ chef-automate upgrade status 

    Automate is upgrading to airgap bundle (2022012400000) 

    $ chef-automate upgrade status 

    Automate is up-to-date with airgap bundle (2022012400000) 
    ```
 

Once upgraded to latest milestone version, you can now perform the major upgrade. Below are the steps for major upgrade to 3.x.x:

- Create AIB Bundle on Internet Connected Machine: `chef-automate airgap bundle create /<path>/automate_v22.0.1.aib --version 3.x.x`
- Copy the CLI (chef-automate) and AIB (automate_v22.0.1.aib) to Air Gapped Machine where Chef Automate is Running. 
- Run Upgrade On Air Gapped Machine: `sudo chef-automate upgrade run --airgap-bundle /<path>/automate_v3.x.x.aib --major `. You will be presented with upgrade information and displayed a checklist of requirements pre upgrade along with a list of steps to perform post upgrade. The steps are listed below :
    ```
    This is a Major upgrade.
    ========================

      1) In this release Embedded PostgreSQL is upgraded to version 13.5
      2) This will need special care if you use Embedded PostgreSQL.

    ===== Your installation is using External PostgreSQL =====

      Please confirm this checklist that you have taken care of these steps
      before continuing with the Upgrade to version 3.0.0:

    You had planned for a downtime?: (y/n)
    y
    You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
    y
    Upgrade your PostgreSQL 9.6 to 13.5 with the help of your Database Administrator (y/n)
    y
    After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
    y

    Post Upgrade Steps:
    ===================

    1) If your PostgreSQL Connection URL and Credential are changed then update them by putting them in config.toml and patching it in using:
        $ chef-automate config patch config.toml

    2) Check the status of your upgrade using:
        $ chef-automate upgrade status
      This should return: Automate is up-to-date

    3) Check all services are running using:
        $ chef-automate status

    4) Check Automate UI everything is running and all data is visible

    **** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

    Now, upgrade will start, Please confirm to continue... (y/n)
    ```
- Check all services are running: `chef-automate status`
- Once all the above steps are complete, major version upgrade is successful.