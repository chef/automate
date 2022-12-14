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

| Parameters                                      | Default Values In Standalone Infra Server | Default Values In Backend Server | Automate Configuration                                     |
| ----------------------------------------------- | ----------------------------------------- | -------------------------------- | ---------------------------------------------------------- |
| nginx['client_max_body_size']                   | 250                                       | 250m                             | ```cs_nginx.v1.sys.ngx.http.client_max_body_size```        |
| nginx['ssl_protocols']                          | TLSv1.2                                   | TLSv1.2                          | ```cs_nginx.v1.sys.ngx.http.ssl_protocols```               |
| nginx['worker_connections']                     | 10240                                     | 10240                            | ```cs_nginx.v1.sys.ngx.events.worker_connections```        |
| nginx['worker_processes']                       | 4                                         | 2                                | ```cs_nginx.v1.sys.ngx.main.worker_processes```            |
| opscode_erchef['s3_url_ttl']                    | 900                                       | 28800                            | ```erchef.v1.sys.api.s3_url_ttl```                         |
| opscode_erchef['auth_skew']                     | 900                                       | 900                              | ```erchef.v1.sys.api.auth_skew```                          |
| opscode_erchef['authz_fanout']                  | 20                                        | 20                               | ```erchef.v1.sys.authz.auth_fanout```                      |
| opscode_erchef['authz_timeout']                 | 2000                                      | 2000                             | ```erchef.v1.sys.authz.auth_timeout```                     |
| opscode_erchef['base_resource_url']             | :host_header                              | :host_header                     | ```erchef.v1.sys.api.base_resource_url```                  |
| opscode_erchef['bulk_fetch_batch_size']         | 5                                         | 5                                | ```erchef.v1.sys.api.bulk_fetch_batch_size```              |
| opscode_erchef['cleanup_batch_size']            | 0                                         | 0                                | ```erchef.v1.sys.authz.cleanup_batch_size```               |
| opscode_erchef['depsolver_timeout']             | 5000                                      | 5000                             | ```erchef.v1.sys.depsolver.timeout```                      |
| opscode_erchef['depsolver_worker_count']        | 5                                         | 5                                | ```erchef.v1.sys.depsolver.pool_init_size```               |
| opscode_erchef['depsolver_pooler_timeout']      | 100000                                    | 0                                | ```erchef.v1.sys.depsolver.pool_queue_timeout```           |
| opscode_erchef['depsolver_pool_queue_max']      | 10                                        | 50                               | ```erchef.v1.sys.depsolver.pool_queue_max```               |
| opscode_erchef['db_pool_size']                  | 40                                        | 20                               | ```erchef.v1.sys.sql.pool_max_size```                      |
| opscode_erchef['db_pool_queue_max']             | 40                                        | 20                               | ```erchef.v1.sys.sql.pool_queue_max```                     |
| opscode_erchef['ibrowse_max_pipeline_size']     | 1                                         | 1                                | ```erchef.v1.sys.ibrowse.max_pipeline_size```              |
| opscode_erchef['ibrowse_max_sessions']          | 256                                       | 256                              | ```erchef.v1.sys.ibrowse.max_sessions```                   |
| opscode_erchef['max_request_size']              | 2000000                                   | 2000000                          | ```erchef.v1.sys.api.max_request_size```                   |
| opscode_erchef['keygen_cache_size']             | 1000                                      | 10                               | ```erchef.v1.sys.keygen.cache_size```                      |
| opscode_erchef['reindex_batch_size']            | 10                                        | 10                               | ```erchef.v1.sys.index.reindex_batch_size```               |
| opscode_erchef['reindex_sleep_min_ms']          | 500                                       | 500                              | ```erchef.v1.sys.index.reindex_sleep_min_ms```             |
| opscode_erchef['reindex_sleep_max_ms']          | 2000                                      | 2000                             | ```erchef.v1.sys.index.reindex_sleep_max_ms```             |
| opscode_erchef['reindex_item_retries']          | 3                                         | 3                                | ```erchef.v1.sys.index.reindex_item_retries```             |
| opscode_erchef['cbv_cache_enabled']             | FALSE                                     | FALSE                            | ```erchef.v1.sys.api.cbv_cache_enabled```                  |
| opscode_erchef['search_queue_mode']             | batch                                     | batch                            | ```erchef.v1.sys.index.search_queue_mode```                |
| oc_chef_authz['http_queue_max']                 | 200                                       | 200                              | ```erchef.v1.sys.authz.pool_queue_max```                   |
| oc_chef_authz['http_max_count']                 | 100                                       | 100                              | ```erchef.v1.sys.authz.pool_max_size```                    |
| oc_chef_authz['http_init_count']                | 100                                       | 100                              | ```erchef.v1.sys.authz.pool_init_size```                   |
| data_collector['timeout']                       | 30000                                     | 30000                            | ```erchef.v1.sys.data_collector.timeout```                 |
| data_collector['http_init_count']               | 25                                        | 25                               | ```erchef.v1.sys.data_collector.pool_init_size```          |
| data_collector['http_max_count']                | 100                                       | 100                              | ```erchef.v1.sys.data_collector.pool_max_size```           |
| data_collector['http_max_age']                  | {70, sec}                                 | {70, sec}                        | ```erchef.v1.sys.data_collector.pool_max_age```            |
| data_collector['http_cull_interval']            | {1, min}                                  | {1, min}                         | ```erchef.v1.sys.data_collector.pool_cull_interval```      |
| data_collector['http_max_connection_duration']  | {70, sec}                                 | {70, sec}                        | ```erchef.v1.sys.data_collector.max_connection_duration``` |
| data_collector['ibrowse_options']               | [{connect_timeout, 10000}]                | [{connect_timeout, 10000}]       | ```erchef.v1.sys.data_collector.ibrowse_timeout```         |
| oc_bifrost['db_pool_queue_max']                 | 50                                        | 50                               | ```bifrost.v1.sys.sql.pool_queue_max```                    |
| oc_bifrost['extended_perf_log']                 | TRUE                                      | TRUE                             | ```bifrost.v1.sys.log.extended_perf_log```                 |
| bookshelf['stream_download']                    | TRUE                                      | TRUE                             | ```bookshelf.v1.sys.bookshelf.stream_download```           |
| oc_chef_wm['health_ping_timeout']               | 400                                      | 400                             | ```erchef.v1.sys.health.health_ping_timeout```           |

Click [here](https://docs.chef.io/server/config_rb_server_optional_settings/) for the detailed description of the above parameters.
