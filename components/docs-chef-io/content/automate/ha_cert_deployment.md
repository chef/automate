+++
title = "Add Certificate During Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Add Certificate During Deployment"
    parent = "automate/deploy_high_availability/certificates"
    identifier = "automate/deploy_high_availability/certificates/ha_cert_deployment.md Add Certificate During Deployment"
    weight = 230
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Custom certificates can be added during deployment for different services.

## Common certificate for all the nodes of a service

The following services support custom certificates:

### Custom certificate for Chef Automate

To add a common custom certificate for all the nodes of Chef Automate service, add the configurations given below in `config.toml` file before running `chef-automate deploy` command:

```toml
[automate.config]
custom_certs_enabled = true
root_ca = "ADD_YOUR_ROOT_CA_CERT_HERE"
private_key = "ADD_YOUR_PRIVATE_KEY_HERE"
public_key = "ADD_YOUR_PUBLIC_KEY_HERE"
```

### Custom certificate for Chef Server

To add a common custom certificate for all the nodes of Chef Server service, add the configurations given below in `config.toml` file before running `chef-automate deploy` command:

```toml
[chef_server.config]
custom_certs_enabled = true
root_ca = "ADD_YOUR_ROOT_CA_CERT_HERE"
private_key = "ADD_YOUR_PRIVATE_KEY_HERE"
public_key = "ADD_YOUR_PUBLIC_KEY_HERE"
```

### Custom certificate for OpenSearch

To add a common custom certificate for all the nodes of OpenSearch service, add the configurations given below in `config.toml` file before running `chef-automate deploy` command:

```toml
[opensearch.config]
custom_certs_enabled = true
root_ca = "ADD_YOUR_ROOT_CA_CERT_HERE"
private_key = "ADD_YOUR_PRIVATE_KEY_HERE"
public_key = "ADD_YOUR_PUBLIC_KEY_HERE"
admin_key = "ADD_YOUR_ADMIN_KEY_HERE"
admin_cert = "ADD_YOUR_ADMIN_CERT_HERE"
```

### Custom certificate for PostgreSQL

To add a common custom certificate for all the nodes of PostgreSQL service, add the configurations given below in `config.toml` file before running `chef-automate deploy` command:

```toml
[postgresql.config]
custom_certs_enabled = true
root_ca = "ADD_YOUR_ROOT_CA_CERT_HERE"
private_key = "ADD_YOUR_PRIVATE_KEY_HERE"
public_key = "ADD_YOUR_PUBLIC_KEY_HERE"
```
