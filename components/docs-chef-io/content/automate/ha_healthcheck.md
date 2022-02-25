+++
title = "HA Health Check Commands"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA Health Check Commands"
    parent = "automate/install"
    identifier = "automate/install/ha_healthcheck.md HA Health Check Commands"
    weight = 230
+++

This page includes commands that you can execute for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

## Log Check Commands

The Chef Automate frontend and backend nodes service logs are available via `journalctl` from each node. You can identify the service by the name in the generated output preceding with the logline.

- Execute the following command, `journalctl --follow --unit hab-sup`, to view the backend logs related to all hab services.

Where the *--unit* displays the logs from the specified unit, and *--follow* means to follow the journal.

- Use the *grep* command to filter the logs related to a specific service. For example, run this command `journalctl --follow --unit hab-sup | grep 'automate-ha-elasticsearch'` to view the log of the habitat component in the Chef Automate frontend node.

- Execute the following command, `journalctl --follow --unit chef-automate`, to view the log of the frontend (chef-automate and chef-server instances) nodes.

- Use the *grep* command to filter the logs for a single service. For example, run this command, `journactl --follow --unit chef-automate | grep ingest.service` to view the ingest logs of the Chef Automate frontend node.

## Health Check Service Commands

- Execute the following command, `chef-automate status`, to SSH the frontend node.

- Execute the following command, `hab svc status`, to SSH the backend node.

- Execute the following command, `hab svc status`, to verify the health of any services on a node,
