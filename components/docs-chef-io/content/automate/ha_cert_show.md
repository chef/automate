+++
title = "View Certificates"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "View Certificates"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_show.md View Certificates"
    weight = 200
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Certificates can be viwed for different services using below commands.

## Viewing certificates of all services

To view certificates of all services, run the following command:

```bash
chef-automate cert show 
```

## Viewing certificates of Automate cluster

To view certificates of Automate cluster, run the following command:

```bash
chef-automate cert show --automate
#or
chef-automate cert show -a
#or
chef-automate cert show --a2
```

## Viewing certificates of Chef Server cluster

To view certificates of Chef Server cluster, run the following command:

```bash
chef-automate cert show --chef_server
#or
chef-automate cert show -c
#or
chef-automate cert show --cs
```

## Viewing certificates of Postgresql cluster

To view certificates of Postgresql cluster, run the following command:

```bash
chef-automate cert show --postgresql
#or
chef-automate cert show -p
#or
chef-automate cert show --pg
```

## Viewing certificates of OpenSearch cluster

To view certificates of OpenSearch cluster, run the following command:

```bash
chef-automate cert show --opensearch
#or
chef-automate cert show -o
#or
chef-automate cert show --os
```

{{< note >}}

- You can also view certificates of a specific node of a service by passing the node's IP address as a flag `--node <IP_ADDRESS>` or `-n <IP_ADDRESS>` to the above commands.

{{< /note >}}
