+++
title = "HA Security and Firewall"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Security and Firewall"
    parent = "automate/install"
    identifier = "automate/install/ha_security_firewall.md HA Security and Firewall"
    weight = 450
+++

Chef Automate High Availability (HA) cluster requires multiple ports for the frontend and backend servers to operate effectively and reduce possible network traffic. Below it a breakdown of those ports and what needs to be open for each set of servers.

## Network Traffic for Incoming Frontend Nodes

| Provisioning server to frontend nodes | |
| --- | --- |
| TCP 22 | Allows terraform to securely access (SSH) your network infrastructure and configure services. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| Users, chef-servers, and chef- clients to frontend nodes | |
| TCP 443 | Allows you to establish a connection with the UI and chef-servers APIs for reporting. Access to this port is required if chef-clients have to report directly or download profiles. |
| TCP 80, optional | Allows you to redirect to port 443. |

## Network Traffic for Incoming Elastic-search Backend Nodes

| Provisioning server to Elasticsearch backend nodes | |
| --- | --- |
| TCP 22 | Allows Terraform to securely access (SSH) your network infrastructure and configure services. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| Admin users to Elasticsearch backend nodes | |
| TCP 5601, optional | Allows you to establish a connection with Kibana on the Elasticsearch servers. The successful connection hosts an operations dashboard displaying metrics for the Elasticsearch and Postgres servers. |
| Frontend nodes to the Elasticsearch backend nodes | |
| TCP 9200 | Allows Chef Automate HA to communicate to the Elasticsearch API. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| Elasticsearch backend nodes to Elasticsearch backend nodes | |
| TCP 9300 | Allows Elasticsearch node to distribute data in its cluster. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between Elasticsearch nodes. |
| TCP 9631 | Allows Habitat API to be reachable from services on the Elasticsearch nodes |
| Postgres backends nodes to Elasticsearch backends nodes and vice versa | |
| TCP 9200 | Allows Metricbeats to send data to the Elasticsearch for use in Kibana. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between all backend nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on all backend nodes. |

## Network Traffic for Incoming PostgreSQL Backend Nodes

| Provisioning server to Postgres backend nodes | |
| --- | --- |
| TCP 22 | Allows Terraform to securely access (SSH) and configure services. |
| TCP 9631 | Allows the tools for configuration to reach the habitat API.  |
| Frontend nodes to Postgres backend nodes | |
| TCP 7432 | Allows Automate to connect to haproxy that forwards to the psql leader. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| Postgres backend nodes to Postgres backend nodes | |
| TCP 5432 | Allows haproxy on Postgres backends to forward connections to the leader. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between Postgres nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on the Postgres nodes. |
| Elasticsearch backend nodes to Postgres backend nodes and vice versa | |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between all backend nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on all backend nodes. |
