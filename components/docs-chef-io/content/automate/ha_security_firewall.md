+++
title = "Security and Firewall"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Security and Firewall"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/install/high_availability/common_features/ha_security_firewall.md Security and Firewall"
    weight = 210
+++

The Chef Automate High Availability (HA) cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Below is a breakdown of those ports and what needs to be open for each set of servers.

## Ports required for all Machines

| Machines | Chef Automate         | Chef Infra Server     | Postgresql                                  | OpenSearch                                  | Bastion      |
|----------|-----------------------|-----------------------|---------------------------------------------|---------------------------------------------|--------------|
| Incoming | TCP 22, 9631, 443, 80 | TCP 22, 9631, 443, 80 | TCP 22, 9631, 7432, 5432, 9683<br/>UDP 9638 | TCP 22, 9631, 9200, 9300, 9638<br/>UDP 9638 |              |
| Outgoing | TCP 22, 9631, 443, 80 | TCP 22, 9631, 443, 80 | TCP 22, 9631, 7432, 5432, 9683<br/>UDP 9638 | TCP 22, 9631, 9200, 9300, 9638<br/>UDP 9638 | TCP 22, 9631 |

## Port usage definitions

| Protocol | Port Number | Usage                                                                                            |
|----------|-------------|--------------------------------------------------------------------------------------------------|
| TCP      | 22          | SSH to configure services                                                                        |
| TCP      | 9631        | Habitat HTTP API                                                        |
| TCP      | 443         | Allow Users to reach UI / API                                                                    |
| TCP      | 80          | Optional, Allows users to redirect to 443                                                        |
| TCP      | 9200        | OpenSearch API HTTPS Access                                                                      |
| TCP      | 9300        | Allows OpenSearch node to distribute data in its cluster.                                        |
| TCP/UDP  | 9638        | Habitat gossip (UDP) |
| TCP      | 7432        | HAProxy, which redirects to Postgresql Leader |
