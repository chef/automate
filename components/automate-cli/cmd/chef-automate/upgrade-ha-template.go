package main

var upgradeHaHelpDoc = `
Usage:
    chef-automate upgrade run
Options:
--upgrade-frontends OPTIONS          uppgrade FE bundle 
--upgrade-backends OPTIONS upgrade BE bundle
--upgrade-airgap-bundles This will upgrade both the bundles
--skip-deploy Adding this will skip deployment and just update bundles 
`