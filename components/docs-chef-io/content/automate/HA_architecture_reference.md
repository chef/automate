+++
title = "HA Reference Architecture"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Reference Architecture"
    parent = "automate/install"
    identifier = "automate/install/ha_architecture_reference.md HA Reference Architecture"
    weight = 210
+++

## Chef Automate High Availability (HA) Architecture

This section includes Chef Automate High Availability (HA) high-level reference architecture that interacts with the HA backend components on different providers or in different environments.

The following Chef Automate HA architecture diagram shows the components involved in the Chef Automate HA that works on **Leader-Follower** strategy. We are creating the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch for Chef Automate HA.

![High Availability Architecture](/images/automate/ha_architecture.png)

All application service, secret, recovery, data are stored in the Postgress. Compliance and client run data that are generated on time to time basis and requiring data to be accessible in real-time are stored in Elastic search. Load balancer distributes to each of the automate components.

The Chef Automate HA architecture involves two different clusters part of the main cluster, which are:

- Automate Backend Cluster and Nodes

The backend components connect into the frontend habitat supervisor cluster. In the habitat supervisor, **postgres** and **elasticsearch** instances runs. A minimum of three nodes is required for **Postgres** and **Elasticsearch** databases, where one becomes a leader, and others are followers.

- Automate Frontend Cluster and Nodes

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with load balancer configurations.

These clusters comprise **four** different servers with HA mode, which are as follows:

1. Chef-automate
2. Chef Infra Server
3. Elasticsearch - an open-source search and analytics engine based on Apache Lucene and built with Java. It is a time-series and NoSQL database that stores data in an unstructured way and is used for indexing purpose.
4. PostgreSQL - an open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

<!-- ! -- These four components reside in a VPC under one network in AWS. Every node sits on a specific machine irrespective of a database. Single database for all three nodes of automate. -->

{{ note >}}

Elastic Search internally manages the communication and backup, and  does not follow any leader-follower stratery.

{{ /note >}}

**Journalbeat** and **Metricbeat** are common for all database instances. **Journalbeat** installed as an agent on the servers collects all the services logs and forwards them to Elasticsearch. **Metricbeat** installed on the servers periodically collects metrics from the operating system and services running on the server and sends them to the **Kibana**.

**Kibana** is an open-source, web-based data visualization and analytical tool that allows you to explore, visualize, and build a dashboard over the log data massed in Elasticsearch clusters. It is a part of the Elastic Stack and integrates with Elasticsearch. The **Kibana** **Dashboard** is a collection of charts, graphs, metrics, searches, and maps in a single pane and provides at-a-glance insights into data from multiple perspectives enabling you to drill down into the details.
