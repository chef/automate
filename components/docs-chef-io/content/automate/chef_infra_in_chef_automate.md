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

This page lists the Chef Infra server keys that can be configured in Standalone Automate and Automate HA.

When operated in a standalone mode, the list contains specific available parameters that a person can patch or modify beyond what the values are in Chef Infra Server. All the parameters have their default values in Chef Infra Server. Now, you can access the same parameters from Chef Automate, which you can patch or change the values from the configuration file.

The list of parameters are as follows:

| Chef Infra Server Key                           | Default Values In Automate (Having Infra Server Package) | Default Values In Automate HA | Automate Configuration                                     |
| ----------------------------------------------- | ----------------------------------------- | ----------------------------------- | ---------------------------------------------------------- |
| nginx['client_max_body_size']                   | 250                                       | 250                                 | ```[cs_nginx.v1.sys.ngx.http]```<br>```client_max_body_size="250"``` |
| nginx['ssl_protocols']                          | TLSv1.2                                   | TLSv1.2                             | ```[cs_nginx.v1.sys.ngx.http]```<br>```ssl_protocols="TLSv1.2"```      |
| nginx['worker_connections']                     | 10240                                     | 10240                               | ```[cs_nginx.v1.sys.ngx.events]```<br>```worker_connections=10240``` |
| nginx['worker_processes']                       | 4                                         | 2                                   | ```[cs_nginx.v1.sys.ngx.main]```<br>```worker_processes=4```     |
| nginx['gzip']                                   | ON                                        |ON                                   |```[cs_nginz.v1.sys.ngx.http]```<br>```gzip= "on"```              |
| nginx['gzip_comp_level']                         |2                                          |2                                    |```[cs_nginz.v1.sys.ngx.http]```<br>```gzip_comp_level=2```       |
| nginx['gzip_http_version']                       |1                                          |1                                    |```[cs_nginz.v1.sys.ngx.http]```<br>```gzip_http_version=1```     |
| nginx['gzip_types']                              |[ 'text/plain','text/css','application/x-javascript','text/xml', 'application/xml','application/xml+rss','text/javascript','application/json']|[ 'text/plain','text/css','application/x-javascript','text/xml', 'application/xml','application/xml+rss','text/javascript','application/json']|```[cs_nginz.v1.sys.ngx.http]```<br>```gzip_types ="[ 'text/plain','text/css','application/x-javascript','text/xml', 'application/xml','application/xml+rss','text/javascript','application/json']"```            |
| nginx['keepalive_timeout']                      |65                                         |65                                   |```[cs_nginz.v1.sys.ngx.http]```<br>```keepalive_timeout=65```      |
| nginx['sendfile']                               |ON                                         |ON                                   |```[cs_nginz.v1.sys.ngx.http]```<br>```sendfile ="on"```            |
| nginx['ssl_ciphers']                            |                                           |ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:AES256-GCM-SHA384:AES:!aNULL:!eNULL:!EXPORT                                     |```[cs_nginz.v1.sys.ngx.http]```<br>```ssl_ciphers=""```            |
| opscode_erchef['s3_url_ttl']                    | 900                                       | 28800                               | ```[erchef.v1.sys.api]```<br>```s3_url_ttl=900```                  |
| opscode_erchef['auth_skew']                     | 900                                       | 900                                 | ```[erchef.v1.sys.api]```<br>```auth_skew=900```                   |
| opscode_erchef['authz_fanout']                  | 20                                        | 20                                  | ```[erchef.v1.sys.authz]```<br>```fanout=20```               |
| opscode_erchef['authz_timeout']                 | 2000                                      | 2000                                | ```[erchef.v1.sys.authz]```<br>```timeout=2000```              |
| opscode_erchef['base_resource_url']             | :host_header                              | :host_header                        | ```[erchef.v1.sys.api]```<br>```base_resource_url="host_header"```   |
| opscode_erchef['bulk_fetch_batch_size']         | 5                                         | 5                                   | ```[erchef.v1.sys.api]```<br>```bulk_fetch_batch_size=5```       |
| opscode_erchef['cleanup_batch_size']            | 0                                         | 0                                   | ```[erchef.v1.sys.authz]```<br>```cleanup_batch_size=0```        |
| opscode_erchef['depsolver_timeout']             | 5000                                      | 5000                                | ```[erchef.v1.sys.depsolver]```<br>```timeout=5000```               |
| opscode_erchef['depsolver_worker_count']        | 5                                         | 5                                   | ```[erchef.v1.sys.depsolver]```<br>```pool_init_size=5```        |
| opscode_erchef['depsolver_pooler_timeout']      | 100000                                    | 0                                   | ```[erchef.v1.sys.depsolver]```<br>```pool_queue_timeout=100000```    |
| opscode_erchef['depsolver_pool_queue_max']      | 10                                        | 50                                  | ```[erchef.v1.sys.depsolver]```<br>```pool_queue_max=10```        |
| opscode_erchef['db_pool_size']                  | 40                                        | 20                                  | ```[erchef.v1.sys.sql]```<br>```pool_max_size=40```               |
| opscode_erchef['db_pool_queue_max']             | 40                                        | 20                                  | ```[erchef.v1.sys.sql]```<br>```pool_queue_max=40```              |
| opscode_erchef['ibrowse_max_pipeline_size']     | 1                                         | 1                                   | ```[erchef.v1.sys.ibrowse]```<br>```ibrowse_max_pipeline_size=1```       |
| opscode_erchef['ibrowse_max_sessions']          | 256                                       | 256                                 | ```[erchef.v1.sys.ibrowse]```<br>```ibrowse_max_sessions=256```            |
| opscode_erchef['max_request_size']              | 4000000                                   | 4000000                             | ```[erchef.v1.sys.api]```<br>```max_request_size=4000000```          |
| opscode_erchef['keygen_cache_size']             | 1000                                      | 10                                  | ```[erchef.v1.sys.keygen]```<br>```cache_size=1000```               |
| opscode_erchef['reindex_batch_size']            | 10                                        | 10                                  | ```[erchef.v1.sys.index]```<br>```reindex_batch_size=10```        |
| opscode_erchef['reindex_sleep_min_ms']          | 500                                       | 500                                 | ```[erchef.v1.sys.index]```<br>```reindex_sleep_min_ms=500```      |
| opscode_erchef['reindex_sleep_max_ms']          | 2000                                      | 2000                                | ```[erchef.v1.sys.index]```<br>```reindex_sleep_max_ms=2000```      |
| opscode_erchef['reindex_item_retries']          | 3                                         | 3                                   | ```[erchef.v1.sys.index]```<br>```reindex_item_retries=3```      |
| opscode_erchef['cbv_cache_enabled']             | FALSE                                     | FALSE                               | ```[erchef.v1.sys.api]```<br>```cbv_cache_enabled=false```           |
| opscode_erchef['search_queue_mode']             | batch                                     | batch                               | ```[erchef.v1.sys.index]```<br>```search_queue_mode="batch"```         |
| opscode_erchef['s3_enabled']             | FALSE                                     | FALSE                                      | ```[erchef.v1.sys.api]```<br>```s3_enabled="true"```         |
| opscode_erchef['s3_bucket_name']             | Bookshelf                                     | Bookshelf                          | ```[erchef.v1.sys.api]```<br>```s3_bucket_name="name"```         |
| opscode_erchef['s3_external_url']             |                                      |                                            | ```[erchef.v1.sys.api]```<br>```s3_external_url="url"```         |
| opscode_erchef['strict_search_result_acls']     |FALSE                               | FALSE                                      |``` [erchef.V1.sys.api]```<br>```strict_search_result_acls= false```|
|opscode_erchef['enable_ibrowse_traces']          |FALSE                                      |FALSE                                |```[erchef.v1.sys.ibrowse]```<br> ```enable_ibrowse_traces = false``` |
|license['nodes']                                 |                                           |                                     |```[erchef.v1.sys.authz]```<br>  ```node_license =```              |
|opscode_erchef['s3_url_expiry_window_size']      | {100, percent}                            |                                     |```[erchef.v1.sys.api]``` <br> ```s3_url_expiry_window_size_percent=100```|
| oc_chef_authz['http_queue_max']                 | 200                                       | 200                                 | ```[erchef.v1.sys.authz]```<br>```pool_queue_max=200```            |
| oc_chef_authz['http_max_count']                 | 100                                       | 100                                 | ```[erchef.v1.sys.authz]```<br>```pool_max_size=100```             |
| oc_chef_authz['http_init_count']                | 100                                       | 100                                 | ```[erchef.v1.sys.authz]```<br>```pool_init_size=100```            |
| data_collector['timeout']                       | 30000                                     | 30000                               | ```[erchef.v1.sys.data_collector]```<br>```timeout=30000```          |
| data_collector['http_init_count']               | 25                                        | 25                                  | ```[erchef.v1.sys.data_collector]```<br>```pool_init_size=25```   |
| data_collector['http_max_count']                | 100                                       | 100                                 | ```[erchef.v1.sys.data_collector]```<br>```pool_max_size=100```    |
| data_collector['http_max_age']                  | {70, sec}                                 | {70, sec}                           | ```[erchef.v1.sys.data_collector]```<br>```pool_max_age=70``` |
| data_collector['http_cull_interval']            | {1, min}                                  | {1, min}                            | ```[erchef.v1.sys.data_collector]```<br>```pool_cull_interval=1``` |
| data_collector['http_max_connection_duration']  | {70, sec}                                 | {70, sec}                           | ```[erchef.v1.sys.data_collector]```<br>```max_connection_duration=70``` |
| data_collector['ibrowse_options']               | [{connect_timeout, 10000}]                | [{connect_timeout, 10000}]          | ```[erchef.v1.sys.data_collector]```<br>```ibrowse_timeout=10000``` |
| oc_bifrost['db_pool_queue_max']                 | 50                                        | 50                                  | ```[bifrost.v1.sys.sql]```<br>```pool_queue_max=50```                 |
| oc_bifrost['extended_perf_log']                 | TRUE                                      | TRUE                                | ```[bifrost.v1.sys.log]```<br>```extended_perf_log=true```            |
| bookshelf['stream_download']                    | TRUE                                      | TRUE                                | ```[bookshelf.v1.sys.bookshelf]```<br>```stream_download=true```      |
| bookshelf['aws_access_id']                    |                                       |                                 | ```[bookshelf.v1.sys.bookshelf]```<br>```aws_access_id=""```      |
| bookshelf['aws_secret_key']                    |                                       |                                 | ```[bookshelf.v1.sys.bookshelf]```<br>```aws_secret_key=""```      |
| bookshelf['log_rotation']                        |{ 'file_maxbytes' => 104857600, 'num_to_keep' => 10 }|                  | ```[bookshelf.v1.sys.log]```<br> ```rotation_max_bytes =104857600```<br>```rotation_max_files =10```     |
| bookshelf['storage_type']                       | sql                                       | sql                                 | ```[bookshelf.v1.sys.bookshelf]```<br> ```storage_type = "sql"```|
|bookshelf['vip']                                 |127.0.0.1                                  |127.0.0.1                            |```[bookshelf.v1.sys.bookshelf]```<br> ```virtual_ip = "127.0.0.1"```|
| oc_chef_wm['health_ping_timeout']               | 400                                       | 400                                 | ```[erchef.v1.sys.health]```<br>```health_ping_timeout=400```         |

Click [here](https://docs.chef.io/server/config_rb_server_optional_settings/) for the detailed description of a above parameters.
