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

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Custom certificates can be added during deployment for different services.

## Setting common certificates for all the nodes of a service

The following services support custom certificates:

### Common custom certificate for Chef Automate

To add a common custom certificate for all the nodes of the Chef Automate service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[automate.config]
enable_custom_certs = true

# Add Automate Load Balancer root-ca
root_ca = """ADD_YOUR_ROOT_CA_HERE"""

# Add Automate node internal public and private keys
private_key = """ADD_YOUR_PRIVATE_KEY_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_HERE"""
```

### Common custom certificate for Chef Server

To add a common custom certificate for all the nodes of the Chef Server service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[chef_server.config]
enable_custom_certs = true
private_key = """ADD_YOUR_PRIVATE_KEY_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_HERE"""
```

### Common custom certificate for OpenSearch

To add a common custom certificate for all the nodes of the OpenSearch service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[opensearch.config]
enable_custom_certs = true
root_ca = """ADD_YOUR_ROOT_CA_CERT_HERE"""
private_key = """ADD_YOUR_PRIVATE_KEY_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_HERE"""
admin_key = """ADD_YOUR_ADMIN_KEY_HERE"""
admin_cert = """ADD_YOUR_ADMIN_CERT_HERE"""
```

### Common custom certificate for PostgreSQL

To add a common custom certificate for all the nodes of the PostgreSQL service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[postgresql.config]
enable_custom_certs = true
root_ca = """ADD_YOUR_ROOT_CA_CERT_HERE"""
private_key = """ADD_YOUR_PRIVATE_KEY_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_HERE"""
```

## Setting unique certificates for different nodes of a service

For on-prem installations, the following services support unique custom certificates for different nodes:

{{< note >}}

- If you are using unique custom certificates for each node of a service, then you can skip the Public and Private Keys before the `certs_by_ip` section for that service.
- If you are using unique custom certificates for each node of a service, then make sure to define keys for all the IPs for that service. For example, if you are using 3 nodes for PostgreSQL service, then you need to define keys for all 3 IPs using 3 `certs_by_ip` sections.
- If you have defined keys at both the places (common and inside the `certs_by_ip` section), then the keys defined in the `certs_by_ip` section will be used.
- If your are using aws deployment, post provision you will get the ip addresses of the nodes. To add certs_by_ip, first do provision and then add those fields in the config.toml, then run deploy

{{< /note >}}

### Unique custom certificates for Chef Automate

To add unique custom certificates for 2 the nodes of the Chef Automate service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[automate.config]
enable_custom_certs = true

# Add Automate Load Balancer root-ca
root_ca = """ADD_YOUR_ROOT_CA_CERT_HERE"""

[[automate.config.certs_by_ip]]
ip = "ADD_YOUR_FIRST_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_FIRST_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_FIRST_IP_ADDRESS_HERE"""
[[automate.config.certs_by_ip]]
ip = "ADD_YOUR_SECOND_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_SECOND_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_SECOND_IP_ADDRESS_HERE"""
```

### Unique custom certificates for Chef Server

To add unique custom certificates for 2 the nodes of the Chef Server service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[chef_server.config]
enable_custom_certs = true
[[chef_server.config.certs_by_ip]]
ip = "ADD_YOUR_FIRST_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_FIRST_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_FIRST_IP_ADDRESS_HERE"""
[[chef_server.config.certs_by_ip]]
ip = "ADD_YOUR_SECOND_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_SECOND_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_SECOND_IP_ADDRESS_HERE"""
```

### Unique custom certificates for OpenSearch

To add unique custom certificates for 3 of the nodes of the OpenSearch service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[opensearch.config]
enable_custom_certs = true
root_ca = """ADD_YOUR_ROOT_CA_CERT_HERE"""
admin_key = """ADD_YOUR_ADMIN_KEY_HERE"""
admin_cert = """ADD_YOUR_ADMIN_CERT_HERE"""
[[opensearch.config.certs_by_ip]]
ip = "ADD_YOUR_FIRST_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_FIRST_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_FIRST_IP_ADDRESS_HERE"""
[[opensearch.config.certs_by_ip]]
ip = "ADD_YOUR_SECOND_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_SECOND_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_SECOND_IP_ADDRESS_HERE"""
[[opensearch.config.certs_by_ip]]
ip = "ADD_YOUR_THIRD_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_THIRD_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_THIRD_IP_ADDRESS_HERE"""
```

### Unique custom certificates for PostgreSQL

To add unique custom certificates for 3 of the nodes of the PostgreSQL service, add the configurations given below in the `config.toml` file before running the `chef-automate deploy` command:

```toml
[postgresql.config]
enable_custom_certs = true
root_ca = """ADD_YOUR_ROOT_CA_CERT_HERE"""
[[postgresql.config.certs_by_ip]]
ip = "ADD_YOUR_FIRST_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_FIRST_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_FIRST_IP_ADDRESS_HERE"""
[[postgresql.config.certs_by_ip]]
ip = "ADD_YOUR_SECOND_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_SECOND_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_SECOND_IP_ADDRESS_HERE"""
[[postgresql.config.certs_by_ip]]
ip = "ADD_YOUR_THIRD_IP_ADDRESS_HERE"
private_key = """ADD_YOUR_PRIVATE_KEY_OF_THIRD_IP_ADDRESS_HERE"""
public_key = """ADD_YOUR_PUBLIC_KEY_OF_THIRD_IP_ADDRESS_HERE"""
```
