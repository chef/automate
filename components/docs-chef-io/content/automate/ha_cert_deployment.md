+++
title = "Add Custom Certificate During Deployment"
draft = false
gh_repo = "automate"
[menu]

  [menu.automate]
    title = "Add Custom Certificate During Deployment"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_deployment.md Add Custom Certificate During Deployment"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Custom certificates can be added during deployment for different services. Follow the steps below to provide custom certificates using the `config gen` command.

## Setting Standard Certificates for all the Nodes of a Service

During the config generation, the following prompts will ask for custom certificates to provide relevant certificate paths for the services.

### Common Custom Certificate for Chef Automate

To add a standard custom certificate for all the nodes of Chef Automate services, select:

```bash
  Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, or OpenSearch:
    no
  > yes
```

```bash
  Do you have custom certs for Automate Nodes:
  > yes
    no
```

```bash
Provide Private Key file path for Automate Service:
Provide Public Key file path for Automate Service:
```

### Common Custom Certificate for Chef Server

```bash
 Do you have custom certs for Chef Infra Server Nodes:
  > yes
    no
```

```bash
Provide Private Key file path for Chef Infra Server Service:
Provide Public Cert file path for Chef Infra Server Service:
```

### Common Custom Certificate for PostgreSQL

```bash
 Do you have custom certs for Postgresql Services Nodes:
  > yes
    no
```

```bash
Provide Private Key file path for Postgresql Service:
Provide Public Cert file path for Postgresql Service:
Provide Root CA file path for Postgresql Service:
```

### Common Custom Certificate for OpenSearch

```bash
  Do you have custom certs for OpenSearch Nodes:
  > yes
    no
```

```bash
Provide Private Key file path for OpenSearch Service:
Provide Public Cert file path for OpenSearch Service:
Provide Root CA file path for OpenSearch Service:
Provide Admin Cert file path for OpenSearch Service:
Provide Admin Key file path for OpenSearch Service:
```

## Setting Unique Certificates for different Nodes of a Service

For on-prem installations, the following services support unique custom certificates for different nodes:

{{< note >}}

- If you are using unique custom certificates for each node of a service, then you can skip the Public and Private Keys before the `certs_by_ip` section for that service.
- If you are using unique custom certificates for each service node, then make sure to define keys for all the IPs for that service. For example, if you use three nodes for PostgreSQL service, you must define keys for all 3 IPs using 3 `certs_by_ip` sections.
- If you have defined keys at both places (familiar and inside the `certs_by_ip` section), the keys defined in the `certs_by_ip` section will be used.
- If you are using aws deployment, post provision, you will get the IP addresses of the nodes. To add certs_by_ip, do provision and add those fields in the config.toml, then run deploy

{{< /note >}}

To add unique custom certificates for services using the `config gen` command, follow the steps below during the config generation.

```bash
  Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, or OpenSearch:
    no
  > yes
```

### Unique Custom Certificates for Automate

To add unique custom certificates for two the nodes of the Chef Automate service

```bash
  Do you have custom certs for Automate Nodes:
  > yes
    no
  Does each Automate Node have the same Certs:
    yes
  > no

  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for Automate Node on IP: <Node 1 IP>:
  Provide Public Cert file path for Automate Node on IP: <Node 1 IP>:

  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for Automate Node on IP: <Node 2 IP>:
  Provide Public Key file path for Automate Node on IP: <Node 2 IP>:
```

### Unique Custom Certificates for Chef Infra Server

To add unique custom certificates for two the nodes of the Chef Infra Server service

```bash
  Do you have custom certs for Chef Infra Server Nodes:
  > yes
    no
  Does each Chef Infra Server Node have the same Certs:
    yes
  > no

  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for Chef Infra Server Node on IP: <Node 1 IP>:
  Provide Public Key file path for Chef Infra Server Node on IP: <Node 1 IP>:

  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for Chef Infra Server Node on IP: <Node 2 IP>:
  Provide Public Key file path for Chef Infra Server Node on IP: <Node 2 IP>:
```

### Unique Custom Certificates for OpenSearch

To add unique custom certificates for two the nodes of the OpenSearch service

```bash
  Do you have custom certs for OpenSearch Nodes:
  > yes
    no
  Does each OpenSearch Node have the same Certs:
    yes
  > no

  Provide Root CA file path for OpenSearch Service:
  Provide Admin Cert file path for OpenSearch Service:
  Provide Admin Key file path for OpenSearch Service:

  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for OpenSearch Node on IP: <Node 1 IP>:
  Provide Public Key file path for OpenSearch Node on IP: <Node 1 IP>:

  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for OpenSearch Node on IP: <Node 2 IP>:
  Provide Public Key file path for OpenSearch Node on IP: <Node 2 IP>:

  Node IP address: <Node 3 Ip Address>
  Provide Private Key file path for OpenSearch Node on IP: <Node 3 IP>:
  Provide Public Key file path for OpenSearch Node on IP: <Node 3 IP>:
```

### Unique Custom Certificates for PostgreSQL

To add unique custom certificates for two the nodes of the Postgresql service

```bash
  Do you have custom certs for Postgresql Nodes:
  > yes
    no
  Does each Postgresql Node have the same Certs:
    yes
  > no

  Provide Root CA file path for Postgresql Service:
  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for Postgresql Node on IP: <Node 1 IP>:
  Provide Public Cert file path for Postgresql Node on IP: <Node 1 IP>:

  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for Postgresql Node on IP: <Node 2 IP>:
  Provide Public Cert file path for Postgresql Node on IP: <Node 2 IP>:

  Node IP address: <Node 3 Ip Address>
  Provide Private Key file path for Postgresql Node on IP: <Node 3 IP>:
  Provide Public Cert file path for Postgresql Node on IP: <Node 3 IP>:
```
