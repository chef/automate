+++
title = "Automate HA Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Automate HA Upgrade"
    parent = "automate/install"
    identifier = "automate/install/ha_upgrade.md Automate HA Upgrade"
    weight = 230
+++

This page explains the procedure to upgrade the Chef Automate High Availability (HA) frontend and  backend clusters.

Using this below command it will upgrade both the bundles and deploy it on their respective nodes

chef-automate upgrade run --upgrade-airgap-bundles

And in case, if you want to just update bundles but not deploy it. you can use skip-deploy flag

chef-automate upgrade run --upgrade-airgap-bundles --skip-deploy

To upgrade only frontends

Using this below command it will upgrade only frontend bundles and deploy it.

chef-automate upgrade run --upgrade-frontends

And in case, if you want to just update bundles but not deploy it. you can use skip-deploy flag

chef-automate upgrade run --upgrade-frontends --skip-deploy

To upgrade only backends

Using this below command it will upgrade only backend bundles and deploy it.

chef-automate upgrade run --upgrade-backends

And in case, if you want to just update bundles but not deploy it. you can use skip-deploy flag

chef-automate upgrade run --upgrade-backends --skip-deploy