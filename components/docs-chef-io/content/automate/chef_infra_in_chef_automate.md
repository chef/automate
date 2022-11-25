+++
title = "Chef Infra Configuration In Chef Automate"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Configuration In Chef Automate"
    parent = "automate/configure"
    identifier = "automate/configure/chef_infra_in_chef_automate.md Chef Infra Configuration In Chef Automate"
    weight = 80
+++


The Chef Infra server has Chef Automate embedded settings in multiple services. This page lists down the Chef Infra server keys that can be configured in Stanalone Automate and Automate HA.

When operated in a standalone mode, the list contains specific parameters which are available which a person can patch or modify beyond what the values are in Chef Infra Server. All the parameters have their own default values in Chef Infra Server. Now, you can access to the same parameters from Chef Automate which you can patch or you can change the values by overriding that from a configuration file.

The list of parameters are as follows:

| Parameters                                  | Default Values In Standalone Infra Server | Default Values In Backend Server | Automate Configuration                         |
| ------------------------------------------- | ----------------------------------------- | -------------------------------- | ---------------------------------------------- |
| opscode_erchef['db_pool_size']              | 20                                        | 20                               | ```cfg.sql.pool_max_size```                    |
| opscode_erchef['s3_url_ttl']	              | 900                                       | 28800		                         | ```erchef.v1.sys.api.s3_url_ttl```             |
| opscode_erchef['cleanup_batch_size']        |	0                                         | 0		                             | ```erchef.v1.sys.authz.cleanup_batch_size```   |
| opscode_erchef['ibrowse_max_pipeline_size'] |	1	                                        | 1	                               | ```erchef.v1.sys.ibrowse.max_pipeline_size``` |
| opscode_erchef['ibrowse_max_sessions']      | 256	                                      | 256	                             | ```erchef.v1.sys.ibrowse.max_sessions```      |
| oc_bifrost['extended_perf_log']             | true                                      | true                             | ```bifrost.v1.sys.log.extended_perf_log```     |
|
|
|
|
|
|
|
|
|
|

Click [here](https://docs.chef.io/server/ctl_chef_server/) for the detailed description of the above parameters.
