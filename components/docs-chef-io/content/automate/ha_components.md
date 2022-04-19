+++
title = "High Availability Components"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability Components"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_components.md High Availability Components"
    weight = 230
+++

This section lists the **Chef Automate High Availability (HA)** components and their purpose.

## Automate-ha-cluster-ctl

Provides commands such as `automate-cluster-ctl provision/deploy` installed via automate-backend-deployment.

## Automate-ha-ctl

Aids connect the backend (**postgres** and **opensearch**) databases using an automate configuration file and **Terraform** without manual intervention.

## Automate-ha-deployment

Aids in setting up a workspace for Chef Automate HA environment. For example, `/hab/a2_deploy_workspace`. It also includes **terraform** code, some necessary **scripts**, **inspecs**, **tests**, **Makefile** and so on.

## Automate-ha-opensearch

Includes the **opensearch** configuration and builds the **opensearch** package. It is installed in the backend nodes.

## Automate-ha-elasticsidecar

Provides a sidecar service for **automate-backend-opensearch** that reads user's credentials and passwords of the **opensearch** binding and applies it to **OpenSearch** using the **odfe** tooling.

## Automate-ha-haproxy

Aids in sending a request to the leader node and is placed on the **postgres** cluster.

## Automate-ha-pgleaderchk

This component is used in a proxy health check to determine where to route SQL requests. A **golang** service that checks the local **PostgreSQL** instance to view if it's a *Leader*.

## Automate-ha-postgresql

This component is a wrapper package of *core/postgresql13* for Chef Automate that provides a backend **HA PostgreSQL**.
