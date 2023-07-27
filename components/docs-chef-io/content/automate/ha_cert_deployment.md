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

Custom certificates can be added during deployment for different services, to provide custom certificates using `config gen` command follow below steps.

## Setting common certificates for all the nodes of a service

During config generation process, following pompts will ask for custom certificates provide relavent certificate paths for the services.

### Common custom certificate for Chef Automate

To add a common custom certificates for all the node of Chef Automate servies, select 

```bash
  Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, OpenSearch:
    no
  ▸ yes
```
```bash
  Do you have custom certs for Automate Nodes:
  ▸ yes
    no
```
```bash
Provide Private Key file path for Automate Service: 
Provide Public Key file path for Automate Service:
```
### Common custom certificate for Chef Server
```bash
 Do you have custom certs for Chef Infra Server Nodes:
  ▸ yes
    no
```
```bash
Provide Private Key file path for Chef Infra Server Service:
Provide Public Cert file path for Chef Infra Server Service:
```
### Common custom certificate for PostgreSQL
```bash
 Do you have custom certs for Postgresql Services Nodes:
  ▸ yes
    no
```
```bash
Provide Private Key file path for Postgresql Service:
Provide Public Cert file path for Postgresql Service:
Provide Root CA file path for Postgresql Service:
```
### Common custom certificate for OpenSearch
```bash
  Do you have custom certs for OpenSearch Nodes:
  ▸ yes
    no
```
```bash
Provide Private Key file path for OpenSearch Service:
Provide Public Cert file path for OpenSearch Service:
Provide Root CA file path for OpenSearch Service:
Provide Admin Cert file path for OpenSearch Service:
Provide Admin Key file path for OpenSearch Service:
```


## Setting unique certificates for different nodes of a service

For on-prem installations, the following services support unique custom certificates for different nodes:

{{< note >}}

- If you are using unique custom certificates for each node of a service, then you can skip the Public and Private Keys before the `certs_by_ip` section for that service.
- If you are using unique custom certificates for each node of a service, then make sure to define keys for all the IPs for that service. For example, if you are using 3 nodes for PostgreSQL service, then you need to define keys for all 3 IPs using 3 `certs_by_ip` sections.
- If you have defined keys at both the places (common and inside the `certs_by_ip` section), then the keys defined in the `certs_by_ip` section will be used.
- If your are using aws deployment, post provision you will get the ip addresses of the nodes. To add certs_by_ip, first do provision and then add those fields in the config.toml, then run deploy

{{< /note >}}


To add unique custom certificates for servies using `config gen` command, follow below steps during config genration process.

```bash 
  Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, OpenSearch:
    no
  ▸ yes
```
### Unique custom certificates for Automate
To add unique custom certificates for 2 the nodes of the Chef Automate service
```bash
  Do you have custom certs for Automate Nodes:
  ▸ yes
    no
  Do each Automate Node have same Certs:
    yes
  ▸ no
  
  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for Automate Node on IP: <Node 1 IP>:
  Provide Public Cert file path for Automate Node on IP: <Node 1 IP>:
  
  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for Automate Node on IP: <Node 2 IP>:
  Provide Public Key file path for Automate Node on IP: <Node 2 IP>:
```

### Unique custom certificates for Chef Infra Server
To add unique custom certificates for 2 the nodes of the Chef Infra Server service
```bash
  Do you have custom certs for Chef Infra Server Nodes:
  ▸ yes
    no
  Do each Chef Infra Server Node have same Certs:
    yes
  ▸ no
  
  Node IP address: <Node 1 Ip Address>
  Provide Private Key file path for Chef Infra Server Node on IP: <Node 1 IP>:
  Provide Public Key file path for Chef Infra Server Node on IP: <Node 1 IP>:
  
  Node IP address: <Node 2 Ip Address>
  Provide Private Key file path for Chef Infra Server Node on IP: <Node 2 IP>:
  Provide Public Key file path for Chef Infra Server Node on IP: <Node 2 IP>:
```

### Unique custom certificates for Opensearch
To add unique custom certificates for 2 the nodes of the Opensearch service
```bash
  Do you have custom certs for OpenSearch Nodes:
  ▸ yes
    no
  Do each OpenSearch Node have same Certs:
    yes
  ▸ no
  
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

### Unique custom certificates for Postgresql
To add unique custom certificates for 2 the nodes of the Postgresql service
```bash
  Do you have custom certs for Postgresql Nodes:
  ▸ yes
    no
  Do each Postgresql Node have same Certs:
    yes
  ▸ no
  
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