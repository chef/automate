+++
title = "Performance Benchmarks"
date = 2023-07-10T21:33:17-08:00
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Performance Benchmarks"
    identifier = "automate/deploy_high_availability/ha_performance_benchmarks.md Performance Benchmarks"
    parent = "automate/deploy_high_availability"
    weight = 127
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

## Overview

While the total number of nodes is an important measure, the data processing speeds of Chef Automate will be primarily determined by the rate of Chef Client Runs per Minute (CCR/m).  This also includes InSpec if configured to do so. The CCR/m rate can be significantly impacted by adjusting the Chef client's converge interval (how frequently does Chef run) and the `splay` (level of randomization so that all Chef clients don't run at the exact second).

Our calculations below are according to the 60 minute converge interval. Cutting the converge interval or splay will affect the cluster nodes' incoming request rates and processing requirements. From a calculation standpoint, 5,000 nodes at a 30 minute converge interval are equivalent to 10,000 nodes at a 60-minute intermission.

Several factors can increase the load of the Automate HA cluster, including, but not limited to, automated requests to the API endpoints, the number of cookbooks, cookbook versions, and the complexity of the dependencies between cookbooks. When a cluster has been operating for a while, these items can quickly accumulate, and you may find it necessary to periodically remove old cookbook versions that are no longer used or increase the number of resources or hardware to prevent impacting the normal operations of the cluster.

## Performance Benchmarks

The following assumptions were used for calculating performance and system specifications

1. 60-minute Chef Client converge interval with one compliance scan per node daily.

1. Chef Client converge size of approximately 400KB

1. Compliance scan report size of approximately 1MB

1. No additional requests to the Automate API endpoints

1. Data retention period of 30 days for compliance and converge data.

### 5 node cluster deployment

The following specs were used for testing when deploying Automate Cluster using a five node cluster configuration. When multiple database services run on the same system, the performance of the shared system resources becomes even more critical for OpenSearch and PostgreSQL, with storage performance being critical. Both services are very write-heavy, and any slowness from the storage systems can significantly impact both services and lead to issues with excessive requests times on the front ends.

| Nodes  | CCR/m | FE Server Count | Frontend Specs                        | BE Server Count | Backend Specs              |
|--------|-------|-----------------|---------------------------------------|-----------------|----------------------------|
| 10000  | 166   | 2               | 8 vCPU, 16GB (c5.2xlarge)  | 3               | 16 vCPU, 64GB (m5.4xlarge), 5TB SSD  |
| 20000  | 333   | 2               | 8 vCPU, 16GB (c5.2xlarge)  | 3               | 16 vCPU, 64GB (m5.4xlarge), 6TB SSD  |
| 30000  | 500   | 2               | 16 vCPU, 32GB (c5.4xlarge) | 3               | 16 vCPU, 64GB (m5.4xlarge), 11TB SSD |

### Complete cluster deployment

A complete cluster deployment will separate various components of the Automate HA cluster into separate system sets. This is to minimize the impact on performance and prevent failures in the cluster from impacting other services.

#### Front-end Server Information

| Nodes  | CCR/m | Infra FE Server Count | Automate FE Server Count | Frontend Specs             |
|--------|-------|-----------------------|--------------------------|----------------------------|
| 20000  | 333   | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |  
| 60000  | 1000  | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |
| 100000 | 1666  | 3                     | 3                        | 16 vCPU, 32GB (c5.4xlarge) |

#### Back-end Server Information

| Nodes  | OpenSearch Count | OpenSearch Specs                     | PostgreSQL Count | PostgreSQL Specs          |
|--------|------------------|--------------------------------------|------------------|---------------------------|
| 20000  | 3                | 16 vCPU, 64GB (m5.4xlarge), 5TB SSD  | 3                | 8 vCPU, 16GB (c5.2xlarge), 1 TB SSD |
| 60000  | 3                | 16 vCPU, 64GB (m5.4xlarge), 15TB SSD | 3                | 8 vCPU, 16GB (c5.2xlarge), 1 TB SSD |
| 100000 | 5                | 16 vCPU, 64GB (m5.4xlarge), 15TB SSD | 3                | 8 vCPU, 32GB (m5.2xlarge), 1 TB SSD |

## Tuning the Cluster

The following configs can be used to improve the performance of the Automate HA cluster

### Cookbook Version Cache

Cookbook Version Cache is worth enabling if you frequently see long response times from `cookbook_versions` when under load. Enabling this can significantly improve the cluster's performance if there are many cookbooks with old versions.

```toml
[erchef.v1.sys.api]
  cbv_cache_enabled=true
```

### Depsolver workers

With `splay` intervals, you may find insufficient Cookbook depsolver works available to handle all of the requests. Increasing the number of workers will improve the number of requests the cluster can take at once without having them wait in the queue. Suppose there are many older cookbook versions or complex cookbook version dependencies. In that case, it may be necessary to increase the timeout to allow the workers more time to finish the required calculations.

{{< note >}}The `pool_init_size` should never be more than the number of CPU cores on the system.{{< /note >}}

```toml
[erchef.v1.sys.depsolver]
  timeout=5000
[erchef.v1.sys.depsolver]
  pool_init_size=8
  pool_queue_timeout=10000
```

### Connection pools

If a large number of Chef Infra client converges happen in a small window, it will be necessary to increase the number of database connections. This will impact the amount of CPU and ram the system uses, so monitoring that enough resources are available after increasing these values is important.

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

When expanding the Automate HA cluster to handle additional nodes or loads from users, it's preferable to scale the front-end nodes horizontally by adding more nodes rather than increasing the amount of server resources on the nodes. This makes it easier to scale as new nodes are added to Chef Infra and reduces the amount of configuration tuning required on each front-end node.

Tuning the configs to handle additional CPU cores can be time-consuming. It often can lead to bottlenecks or other issues in different parts of the Automate HA cluster.
