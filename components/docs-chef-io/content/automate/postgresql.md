+++
title = "PostgreSQL"

date = 2022-01-04T12:09:09-08:00
draft = false

[menu]
  [menu.automate]
    title = "Configure Postgresql"
    identifier = "automate/configuring_automate/postgresql/postgresql.md Postgresql"
    parent = "automate/configuring_automate/postgresql"
    weight = 10
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/postgresql.md)

You can configure Chef Automate to use external PostgreSQL clusters that are not deployed via Chef Automate itself.

## Configuring an External PostgreSQL Database

These configuration directions are intended for in the initial deployment of Chef Automate.

Add the following settings to your `config.toml`:

```toml
[global.v1.external.postgresql]
enable = true
nodes = ["<pghostname1>:<port1>", "<pghostname2>:<port2>", "..."]

# To use postgres with SSL, uncomment and fill out the following:
# [global.v1.external.postgresql.ssl]
# enable = true
# root_cert = """$(cat </path/to/root/cert.pem>)"""

[global.v1.external.postgresql.auth]
scheme = "password"

# Create these postgres users before starting the Automate deployment;
# Automate assumes they already exist.
[global.v1.external.postgresql.auth.password.superuser]
username = "<admin username>"
password = "<admin password>"
[global.v1.external.postgresql.auth.password.dbuser]
username = "<dbuser username>"
password = "<dbuser password>"

[global.v1.external.postgresql.backup]
enable = true
```

## Adding Resolvers for PostgreSQL Database

In case you want to resolve the PostgreSQL cluster node IPs dynamically using DNS servers, you can add resolvers/nameservers to the configuration.

Name Servers can be added in two ways:

1. **Add nameserver IPs:** If you are aware of the nameservers which should resolve the PostgreSQL nodes, the nameservers can be added to your `config.toml` file.

    ```toml
    [pg_gateway.v1.sys.resolvers]
      # Multiple resolvers can be specified by adding the resolvers in the list.
      nameservers = ["127.0.0.53:53"]
    ```

1. **Set system DNS entries:** To use existing system nameserver entries from `/etc/resolv.conf`, add the following setting to `config.toml`:

    ```toml
    [pg_gateway.v1.sys.resolvers]
      enable_system_nameservers = true
    ```

If both options are set, nameserver IPs takes precedence over the system nameserver entries.

Apply the changes:

```bash
sudo chef-automate config patch config.toml
````

If you wish to reset to the default configuration or to modify the configuration:

1. Run `chef-automate config show config.toml`.
1. Edit `config.toml` to replace/edit the `pg_gateway.v1.sys.resolvers` section with the configuration values.
1. Run `chef-automate config set config.toml` to apply your changes.
