+++
title = "Chef Sever Migration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Sever Migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef-server-migration.md Chef Server Migration"
    weight = 40
+++

## Knife EC Backup

*knife-ec-backup* is the command to take backup of Chef Server/Backend during Chef Server Migration. *knife-ec-backup* can backup and restore the data in a Chef Infra Server installation and preserves the data in an intermediate and editable text mode. It is similar to the `knife download` and `knife upload` commands and uses the same underlying libraries, but also includes workarounds for objects not yet supported by those tools and various Infra Server API deficiencies.

### Prerequisites

* Chef Infra Client 11.8 and above.

### Installation

#### Chef Infra Server (Recommended)

This gem is installed with Chef Infra Server 12 and later and the sub-commands are available with embedded copy of `knife`. Refer to the command shown below:

```cmd
sudo /opt/opscode/bin/knife ec backup ~/chef-server-backup-directory
```

If you need a newer version of `knife-ec-backup` you can install it using the embedded `gem` command as shown below:

```cmd
/opt/opscode/embedded/bin/gem install knife-ec-backup --no-doc
```

#### Chef WorkStation Install (Unsupported)

On systems other than the Chef Infra Server, installation of the `gem` is not tested or supported. However, if you attempt to do so you will need the PostgreSQL libraries installed.

For example, on macOS:

```cmd
brew install libpq
gem install knife-ec-backup -- --with-pg-config=/usr/local/Cellar/libpq/9.2/bin/pg_config
```

The current location of `pg_config` can be determined with brew info `libpq`.
