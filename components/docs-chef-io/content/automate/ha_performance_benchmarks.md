+++
title = "Performance Benchmarks"

date = 2023-07-10T21:33:17-08:00
draft = false

[menu]
  [menu.automate]
    title = "Performance Benchmarks"
    identifier = "automate/deploy_high_availability/troubleshooting/ha_performance_benchmarks.md Performance Benchmarks"
    parent = "automate/deploy_high_availability/troubleshooting"
    weight = 10
+++

## Overview

While the total number of nodes is an important measure, the data processing speeds of Chef Automate will be primarily determined by the rate of Chef Client Runs per Minute (CCR/m).  This also includes InSpec if configured to do so.  The CCR/m rate can be greatly impacted by adjusting the Chef client’s converge interval (how frequently does Chef run) and the `splay` (level of randomization so that all chef clients don’t run at the exact same second). 

Our calculations below are based on a 60 minute converge interval. Cutting the converge interval or splay will affect the incoming request rates and processing requirements for the cluster nodes. From a calculation standpoint, it is simplest to say that 5,000 nodes at a 30 minute converge interval are equivalent to 10,000 nodes at a 60-minute interval. 

There are several factors that can increase the load of the Automate HA cluster including, but not limited to, automated requests to the API endpoints, the number of cookbooks, cookbook versions, and the complexity of the dependencies between cookbooks. When a cluster has been operating for a while these items can quickly accumulate and you may find it necessary to periodically remove old cookbook versions that are no longer used or increase the amount of resources or hardware to prevent impacting normal operations of the cluster.

## Performance Benchmarks

The following assumptions were used for calculating performance and system specifications

1. 60-minute Chef Client converge interval with 2 Compliance scans per day.
1. Chef Client converge size of aproximately 400KB
1. Compliance scan report size of aproximately 1MB
1. No additional requests to the Automate API endpoints
 
### Front-end Server Information

| Nodes  | CCR/m | Infra FE Server Count | Automate FE Server Count | Frontend Specs             |
|--------|-------|-----------------------|--------------------------|----------------------------|
| 20000  | 333   | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |  
| 60000  | 1000  | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |
| 100000 | 1666  | 3                     | 3                        | 16 vCPU, 32GB (c5.4xlarge) |

### Back-end Server Information

| Nodes  | OpenSearch Count | Opensearch Specs           | PostgreSQL Count | PostgreSQL Specs          |
|--------|------------------|----------------------------|------------------|---------------------------|
| 20000  | 3                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 16GB (c5.2xlarge) | 
| 60000  | 3                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 16GB (c5.2xlarge) |
| 100000 | 5                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 32GB (m5.2xlarge) |

## Tuning the Cluster

The following configs can be used to improve the performance of the Automate HA cluster

### Cookbook Version Cache

If you frequently see very long response times from cookbook_versions when under load, this is worth enabling. Enabling this can significantly improve performance of the cluster if there are a large number of cookbooks with old versions.

```toml
[erchef.v1.sys.api] 
  cbv_cache_enabled=true 
```

### Depsolver workers

With `splay` intervals you may find that there are not enough Cookbook depsolver works available to handle all of the requests. By increasing the number of workers this will improve the number of requests cluster can handle at once without having them wait in the queue. If there are a large number of older cookbook versions or complex cookbook version dependencies then it may be nessecary to increase the timeout to allow the workers more time to finish the necessary calculations.

*Note: The `pool_init_size` should never be more than the number of cpu cores on the system.*

```toml
[erchef.v1.sys.depsolver] 
  timeout=5000 
[erchef.v1.sys.depsolver] 
  pool_init_size=8 
  pool_queue_timeout=10000 
```

### Connection pools

If there are a large number of Chef Infra client converges that happen in a small window it will be necessary increase the number of database connections. This will have an impact on the amount of cpu and ram used by the system so it's important to monitor that enough resources are available after increasing these values.

```toml
[erchef.v1.sys.data_collector] 
  pool_init_size=100 
  pool_max_size=100 

[erchef.v1.sys.sql] 
  timeout = 5000 
  pool_init_size = 40 
  pool_max_size = 40 
  pool_queue_max = 80 
  pool_queue_timeout = 10000 

[bifrost.v1.sys.sql] 
  timeout = 5000 
  pool_init_size = 40 
  pool_max_size = 40 
  pool_queue_max = 80 
  pool_queue_timeout = 10000 

[erchef.v1.sys.authz] 
  timeout = 5000 
  pool_init_size = 100 
  pool_max_size = 100 
  pool_queue_max = 200 
  pool_queue_timeout = 10000 
```

## Final Notes

When it becomes necessary to expand the Automate HA cluster inorder to handle additional nodes or loads from users it’s preferable to scale the front-end nodes horizontally by adding more nodes rather than increasing the amount of server resources on the nodes. This makes it easier to not only scale as new nodes are added to Chef Infra but reduces the amount of configuration tuning required on each individual front-end node. Attempting to tune the configs to handle additional CPU cores can be quite time consuming and often can lead to bottlenecks or other issues in different parts of the Automate HA cluster.

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/ha_performance_benchmarks.md)
