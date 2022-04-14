+++
title = "High Availability Features"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "High Availability Features"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_features.md High Availability Features"
    weight = 210
+++

This section lists the features offered by Chef Automate HA.

## Performance and Scalability

**Scalability** refers to an application's ability to handle the increased workload or expand in response to an increased demand for database access, processing, networking, or system resources.

**Performance** means system throughput under a given workload for a specific timeframe validated by testing the scalability and the reliability of hardware, software, and network. It is an ongoing process and not a result. Performance requirements undergo massive changes as features and functionalities are added and eliminated to accommodate evolving business requirements.

Following guidelines are adhered to achieve a robust degree of high availability on Chef Automate:

- Chef Automate Cluster allows customers to have network infrastructure comprising more than 10,000 nodes while maintaining high performance and scalability.

- Proper measures are employed while designing scalability and performance so that Chef Automate ingests reporting data from Chef Infra nodes and Chef InSpec conveys efficiently.

- Ingesting a report uses several services as the critical path of the Chef data must be monitored and tuned to reduce the data processing bottlenecks.

- Several methods are followed in performance tuning, hardware planning, data retention policies, storage, network load-balancing, and firewall services.

## Load Balancer

**Load balancer** aids in identifying possible failure points and thereby helps in reducing downtime. More efficient workload distribution helps optimize network infrastructure components and increases application availability.

When the Chef Automate HA system with Load Balancer detects server failure, it automatically redistributes workloads to servers or other components, allowing operation continuation. Load balancing helps improve availability, helps incremental scalability, and supports increased levels of fault tolerance.

It achieves optimal operational performance through either a single-node deployment or through a deployment across a cluster. A single load-balancing controller performs all administrative functions and all analytics data gathering and processing in a single-node deployment. Whereas, in a high availability load balancing cluster, additional nodes provide node-level redundancy for the load-balancing controller and maximize performance for CPU-intensive analytics functions.

## Disaster Recovery (DR)

**Disaster Recovery (DR)** is a comprehensive plan for recovering critical operations and systems after catastrophic events. Chef Automate HA focuses on serious but more typical failures, such as a failing component or server. A DR plan may cope with the loss of an entire region.

Chef Automate HA promotes the DR cluster through regular backup and restores measures syncing the data from the production cluster to the DR cluster. Typically, these two clusters are located in different data centers or cloud provider regions, enabling a production environment in a short period with minimal data loss.

## Failover System

Chef Automate HA enables the ability of the system to continue functioning even after the failure of one or more servers. As part of HA failover, the client applications can continue to operate with minimal downtime, which refers to the ability of client connections to migrate from one server to another.

Chef Automate Cluster allows you to minimize downtime by utilizing redundant systems in case of a failure or maintenance. It includes HA capabilities for Automate web services and the associated database services, which are:

### Automate Frontend

Both Chef Automate and Chef Infra Server have a load balancer with a UI. For example, we have three Chef Automate and Chef Server instances. If any of the Chef Automate or Chef Infra Server instances fails to operate, then the traffic is distributed between the rest of the two servers. Thus, you will never experience any downtime, or the amount of downtime will be minimal.

### Automate Backend

The **OpenSearch** and **PostgreSQL** database instances act as automated backend components. Chef habitat's hab supervisor concept makes a cluster for the database instance. Automate backend cluster rests in the habitat ring. For **Postgresql**, **pgleaderchk** service runs in all the **PostgreSQL** instances and ensures to choose a leader in case the leader database fails. For **OpenSearch**, there is a **msae** concept of leader-follower, and for any database failure, a leader election occurs, and a new leader is chosen.
