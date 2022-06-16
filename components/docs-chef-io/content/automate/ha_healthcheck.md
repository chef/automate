+++
title = "Health Check Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Health Check Commands"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_healthcheck.md Health Check Commands"
    weight = 90
+++

This page includes commands that can be executed for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

## Log Check Commands

The Chef Automate frontend and backend nodes service logs are available via `journalctl` from each node. You can identify the service by the name in the generated output preceding the logline.

- Execute the `journalctl --follow --unit hab-sup` command to view the backend logs related to all hab services. Here, *--unit* displays the logs from the specified unit, and *--follow* means to follow the journal.

- Use the *grep* command to filter the logs related to a specific service. For example, execute the `journalctl --follow --unit hab-sup | grep 'automate-ha-elasticsearch'` command to view the log of the habitat component in the Chef Automate frontend node.

- Execute the `journalctl --follow --unit chef-automate` command to view the log of the frontend (chef-automate and chef-server instances) nodes.

- Use the *grep* command to filter the logs for a single service. For example, run the `journactl --follow --unit chef-automate | grep ingest.service` command to view the ingest logs of the Chef Automate frontend node.

## Health Check Service Commands

- Execute the `chef-automate status`, command to SSH the frontend node.

- Execute the `hab svc status`, command to SSH the backend node.

- Execute the `hab svc status`, command to verify the health of any services on a node.

 {{< figure src="/images/automate/ha_status.png" alt="HA Nodes Status">}}
