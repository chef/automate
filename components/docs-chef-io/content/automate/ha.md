+++
title = "High Availability"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "High Availability"
    parent = "automate/install"
    identifier = "automate/install/ha.md High Availability"
    weight = 10
+++

**High availability (HA)** refers to a system or application (such as a network, a server array, or cluster) that offers a high level of operational performance and quality over a relevant time with maximum potential uptime and accessibility for the content stored on it.

While a more basic system will be adequate to serve content to a low or medium number of users, it may include a single point of failure. This means that if one server goes down, whether due to traffic overload or any number of other issues, the entire site or application could become unavailable.

HA simply means the application remains available with no interruption. We achieve high availability when an application continues to operate when one or more underlying components fail. For example, a router, switch, firewall, or server that fails.

Thus, HA is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component.

*This para required* \-- ? "Availability" includes two periods of time: how much time a service is accessible and how much time the system needs to respond to user requests. When it comes to measuring availability, several factors are salient. These include recovery time and both scheduled and unscheduled maintenance periods. Typically, availability as a whole is expressed as a percentage of uptime defined by service level agreements (SLAs). A score of 100 percent characterizes a system that never fails or experiences zero downtime by being 100% operational.

## What are High Availability (HA) clusters?

A **cluster** is a group of inter-connected computers that work together to perform intensive tasks. In a cluster, each computer is referred to as a "node".

**HA clusters** are servers grouped to operate as a single, unified system supporting server applications that can be reliably utilized with a minimum amount of downtime. They are also called **failover clusters** as they share the same storage but use a different network and can run the same workloads of the primary system they support. HA clusters are tested regularly to confirm nodes are always in operational mode.

If a server in the cluster fails, another server or node can take over immediately to help ensure the application or service supported by the cluster remains operational. HA clusters help ensure no single point of failure for critical IT and reduce or eliminate downtime.

Thus, HA clusters strive to support the system or application or services run reliably with minimal downtime.

## Chef Automate High Availability (HA)

The Chef Automate HA equates to reliability, intending to increase functionality, efficiency, and productivity. It is built on the following three characteristics, **Redundancy**, **Monitoring**, and **Failover**. The Chef Automate HA architecture is an approach of defining the components, modules, or implementation of services of a system that ensures optimal operational performance, even at times of high loads. It aids in addressing three major issues, server failure, zone failure, and cloud failure. It also allows you to automate and test everything in your network.

### What are Chef Automate Clusters?

The Chef Automate Clusters is a professional services solution offering installation, high availability, system uptime/ scale-out performance, maintenance, and disaster recovery capabilities. It includes the Chef Infra Server API to simplify the Chef Infrastructure and is built for customers with more than 10,000 chef-client nodes. You can configure it in the private data center or preferred cloud.

### Chef Automate High Availability (HA) Features

This section lists the features offered by Chef Automate HA.

#### Performance and Scalability

Is this required?

Scalability refers to an application's ability to handle the increase in workload or expand in response to an increased demand for database access, processing, networking, or system resources.

Is this required?

Performance means system throughput under a given workload for a specific timeframe that is validated by testing the scalability and the reliability of hardware, software, and network. It is an ongoing process and not an end result. Performance requirements undergo massive changes as features and functionalities are added and eliminated to accommodate evolving business requirements.

Following guidelines are adhered to achieve a robust degree of high availability on Chef Automate:

- Chef Automate Cluster allows customers to have network infrastructure comprising more than 10,000 nodes while maintaining high performance and scalability.

- Proper measures are employed while designing scalability and performance so that Chef Automate ingests reporting data from Chef Infra nodes and Chef InSpec reports efficiently.

- Ingesting a report uses several services as the critical path of the Chef data must be monitored and tuned to reduce the data processing bottlenecks.

- Several methods are followed in performance tuning, hardware planning, data retention policies, storage, network load-balancing, and firewall services.

- As Chef Automate Cluster is offered as a Chef Professional Service, the customer receives expert architecture planning, system architecture recommendations, and performance tuning for their network infrastructure.

#### Load Balancer

Load balancer aids in identifying possible failure points and thereby helps in reducing downtime. More efficient workload distribution helps optimize network infrastructure components and increases application availability.

When the Chef Automate HA system with Load Balancer detects server failure, it automatically redistributes workloads to servers or other components, allowing operation continuation. Load balancing helps improve availability and helps provide incremental scalability, and supports increased levels of fault tolerance.

It achieves optimal operational performance through either a single-node deployment or through a deployment across a cluster. In a single-node deployment, a single load-balancing controller performs all administrative functions and all analytics data gathering and processing. Whereas, in a high availability load balancing cluster, additional nodes provide node-level redundancy for the load-balancing controller and maximize performance for CPU-intensive analytics functions.

#### Disaster Recovery (DR)

Disaster Recovery (DR) is a comprehensive plan for recovering critical operations and systems after catastrophic events. Chef Automate HA focuses on serious but more typical failures, such as a failing component or server. A DR plan may cope with the loss of an entire region.

Chef Automate HA promotes the DR cluster through regular backup and restores measures syncing the data from the production cluster to the DR cluster. Typically, these two clusters are located in different data centers or cloud provider regions enabling a production environment in a short period with minimal data loss.

#### Failover System

Chef Automate HA enables the ability of the system to continue functioning even after the failure of one or more servers. A part of HA failover, the client applications can continue to operate with minimal downtime, which refers to the ability for client connections to migrate from one server to another in the event of server failure.

Chef Automate Cluster allows you to minimize downtime by utilizing redundant systems in case of a failure or maintenance. It includes HA capabilities for Automate web services and the associated database services, which are:

- Automate Frontend

Both Chef Automate and Chef Infra Server have a load balancer with a UI. For example, let's say we have three Chef Automate and Chef Server instances. If any of the Chef Automate or Chef Infra Server instances fails to operate, then the traffic is distributed between the rest of the two servers. Thus, you will never experience any downtime, or the amount of downtime would be minimal.

- Automate Backend

The **Elasticsearch** and **PostgreSQL** database instances act as an automated backend component. Chef habitat's hab supervisor concept is used to make a cluster for the database instance. Automate backend cluster rests in the habitat ring. For **Postgresql**, **pgleaderchk** service runs in all the **PostgreSQL** instances and ensures to choose a leader in case the leader database fails. For **Elasticsearch**, there is a **msae** concept of leader-follower, and for any database failure, a leader election occurs, and a new leader is chosen.
