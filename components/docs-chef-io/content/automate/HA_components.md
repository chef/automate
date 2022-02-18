+++
title = "High Availability (HA) Components"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability (HA) Components"
    parent = "automate/install"
    identifier = "automate/install/ha_components.md High Availability (HA) Components"
    weight = 210
+++

This section lists the Chef Automate High Availability (HA) components and their purpose.

## Automate-ha-cluster-ctl

Provides commands such as `automate-cluster-ctl provision/deploy`, which is installed via automate-backend-deployment.

## Automate-ha-ctl

Aids connect the backend (**postgres** and **elasticsearch**) databases using an automate configuration file and **Terraform** without any manual intervention.

## Automate-ha-curator

**Elasticsearch** curator aids in curating and managing the **Elasticsearch** indices and snapshots by obtaining the entire actionable list of indices (or snapshots) from the cluster. This component is the same as the default curator. It's written in a **hab** package to merge applications in a hab environment.

## Automate-ha-deployment

Aids in setting up a workspace for Chef Automate HA environment. For example, `/hab/a2_deploy_workspace`. It also includes **terraform** code, some necessary **scripts**, **inspecs**, **tests**, **Makefile** and so on.

## Automate-ha-elasticsearch

Includes the **elasticsearch** configuration and builds the **elasticsearch** package. It is installed in the backend nodes.

## Automate-ha-elasticsidecar

Provides a sidecar service for **automate-backend-elasticsearch** that reads user's credentials and passwords of the **elasticsearch** binding and applies it to **Elasticsearch** using the **odfe** tooling.

## Automate-ha-haproxy

Aids in sending a request to the leader node and is placed on **postgres** cluster.

## Automate-ha-kibana

Aids in viewing logs at a central place. The **Kibana Dashboard** displays the **postgres** and **elasticsearch** nodes with system metrics such as RAM and CPU details, and services logs such as **pgleaderchk**, **curator**.

## Automate-ha-journalbeat

Aids in collecting **journalctl** logs like all the service logs. It is placed on all **postgres** and **elasticsearch** nodes. It collects and sends the log information to **elasticsearch**, and the **Kibana Dashboard** displays the respective log information.

## Automate-ha-metricbeat

This component is placed on all **postgres** and **elasticsearch** nodes. It collects all related metrics and sends them to
**elasticsearch**. The **Kibana Dashboard** displays the respective log information.

## Automate-ha-pgleaderchk

This component is used in a proxy health check to determine where to route SQL requests. A **golang** service that checks the local **PostgreSQL** instance to view if it's a *Leader*.

## Automate-ha-postgresql

This component is a wrapper package of *core/postgresql11* for Chef Automate that provides a backend **HA PostgreSQL**.
