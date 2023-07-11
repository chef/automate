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

Additionaly performing requests to the Automate API endpoints either manually or through some scripted process can greatly impact the load on the Front-end servers and may prequire additional servers or resources in order to handle these requests while processing normal Chef Client and Compliance requests.

## Performance Benchmarks

The following assumptions were used for calculating performance and system specifications

1. 60-minute Chef Client converge interval with 2 Compliance scans per day.
1. Chef Client converge size of aproximately 400KB
1. Compliance scan report size of aproximately 1MB
1. No additional requests to the Automate API endpoints
 
## Front-end Server Information

| Nodes  | CCR/m | Infra FE Server Count | Automate FE Server Count | Frontend Specs             |
|--------|-------|-----------------------|--------------------------|----------------------------|
| 20000  | 333   | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |  
| 60000  | 1000  | 2                     | 2                        | 8 vCPU, 16GB (c5.2xlarge)  |
| 100000 | 1666  | 3                     | 3                        | 16 vCPU, 32GB (c5.4xlarge) |

## Back-end Server Information

| Nodes  | OpenSearch Count | Opensearch Specs           | PostgreSQL Count | PostgreSQL Specs          |
|--------|------------------|----------------------------|------------------|---------------------------|
| 20000  | 3                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 16GB (c5.2xlarge) | 
| 60000  | 3                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 16GB (c5.2xlarge) |
| 100000 | 5                | 16 vCPU, 64GB (m5.4xlarge) | 3                | 8 vCPU, 32GB (m5.2xlarge) |

## Final Notes

When it becomes necessary to expand the Automate HA cluster inorder to handle additional nodes or loads from users it’s preferable to scale the front-end nodes horizontally by adding more nodes rather than increasing the amount of server resources on the nodes. This makes it easier to not only scale as new nodes are added to Chef Infra but reduces the amount of configuration tuning required on each individual front-end node. Attempting to tune the configs to handle additional CPU cores can be quite time consuming and often can lead to bottlenecks or other issues in different parts of the Automate HA cluster.

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/ha_performance_benchmarks.md)
