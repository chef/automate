package main

var upgradeHaHelpDoc = `
Usage:
    chef-automate upgrade run < Options >
Options:
--upgrade-frontends OPTIONS uppgrade FE bundle 
--upgrade-backends OPTIONS upgrade BE bundle
--upgrade-airgap-bundles This will upgrade both the bundles
--skip-deploy Adding this will skip deployment and just update bundles 

eg: chef-automate upgrade run --upgrade-frontends
chef-automate upgrade run --upgrade-frontends --skip-deploy
`
