+++
title = "Security and Firewall"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Security and Firewall"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/ha_security_firewall.md Security and Firewall"
    weight = 210
+++

Chef Automate High Availability (HA) cluster requires multiple ports for the frontend and backend servers to operate effectively and reduce possible network traffic. Below is a breakdown of those ports and what needs to be open for each set of servers.

## Network Traffic for Incoming Frontend Nodes

| Provisioning server to frontend nodes | |
| --- | --- |
| TCP 22 | Allows terraform to securely access (SSH) your network infrastructure and configure services. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| **Users, Chef-Servers, and Chef-Clients to Frontend Nodes** | |
| TCP 443 | Allows you to establish a connection with the UI and chef-servers APIs for reporting. Access to this port is required if chef-clients have to report directly or download profiles. |
| TCP 80, optional | Allows you to redirect to port 443. |

## Network Traffic for Incoming OpenSearch Backend Nodes

| Provisioning server to OpenSearch backend nodes | |
| --- | --- |
| TCP 22 | Allows Terraform to securely access (SSH) your network infrastructure and configure services. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| **Admin users to OpenSearch Backend Nodes** | |
| TCP 5601, optional | Allows you to establish a connection with Kibana on the OpenSearch servers. The successful connection hosts an operations dashboard displaying metrics for the OpenSearch and Postgres servers. |
| Frontend nodes to the OpenSearch backend nodes | |
| TCP 9200 | Allows Chef Automate HA to communicate to the OpenSearch API. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| **OpenSearch Backend Nodes to OpenSearch Backend Nodes** | |
| TCP 9300 | Allows OpenSearch node to distribute data in its cluster. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between OpenSearch nodes. |
| TCP 9631 | Allows Habitat API to be reachable from services on the OpenSearch nodes |
| **Postgres Backend Nodes to OpenSearch Backend Nodes and Vice-Versa** | |
| TCP 9200 | Allows Metricbeats to send data to the OpenSearch for use in Kibana. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between backend nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on all backend nodes. |

## Network Traffic for Incoming PostgreSQL Backend Nodes

| Provisioning server to Postgres backend nodes | |
| --- | --- |
| TCP 22 | Allows Terraform to securely access (SSH) and configure services. |
| TCP 9631 | Allows the tools for configuration to reach the habitat API.  |
| **Frontend Nodes to Postgres Backend Nodes** | |
| TCP 7432 | Allows Automate to connect to haproxy that forwards to the psql leader. |
| TCP 9631 | Allows the tools to query information from the backend to configure Chef Automate HA. |
| **Postgres Backend Nodes to Postgres Backend Nodes** | |
| TCP 5432 | Allows haproxy on Postgres backends to forward connections to the leader. |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between Postgres nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on the Postgres nodes. |
| **OpenSearch Backend Nodes to Postgres Backend Nodes and Vice-Versa** | |
| TCP/UDP 9638 | Allows Habitat to communicate configuration changes between backend nodes. |
| TCP 9631 | Allows the Habitat API to be reachable from services on all backend nodes. |
