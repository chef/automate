+++
title = "Chef Infra Sever Migration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Server Migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef-infra-server-migration.md Chef Infra Server Migration"
    weight = 40
+++

## Knife EC Backup

*knife-ec-backup* is the command to take backup of Chef Infra Server/Backend during Chef Infra Server Migration. *knife-ec-backup* can backup and restore the data in a Chef Infra Server installation and preserves the data in an intermediate and editable text mode. It is similar to the `knife download` and `knife upload` commands and uses the same underlying libraries, but also includes workarounds for objects not yet supported by those tools and various Infra Server API deficiencies.

### Prerequisites

* Chef Infra Server 11.8 and above.

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

### Run the `knife-ec-backup` Command

Run the `knife-ec-backup` command when you are logged in to your Chef Server machine.

For example: `/opt/opscode/bin/knife ec backup --server-url https://ec2-18-xxx-112-xxx.us-east-2.compute.amazonaws.com ./backup/ --with-user-sql --with-key-sql -c /etc/opscode/pivotal.rb`.

Refer to some supported commands below:

* `--sql-host`: The hostname of the Chef Infra Server's PostgreSQL Server. (default: `localhost`).

* `--sql-port`: The PostgreSQL listening port on the Chef Infra Server. (default: `5432`).

* `--sql-db`: The PostgreSQL Chef Infra Server database name. (default: `opscode_chef`) Specify `automate-cs-oc-erchef` when using Automate Chef Infra Server API.

* `--sql-user`: The username of PostgreSQL user with access to the `opscode_chef` database. (default: `autoconfigured` from */etc/opscode/chef-server-running.json*)

* `--sql-password`: The password for the `sql-user`. (default: `autoconfigured` from */etc/opscode/chef-server-running.json*)

### Output of the Command

The command takes the backup of the CS in the allotted folder. The folder structure is: Here is the folder structure: `drwxr-xr-x 5 root root 4096 Dec 28 14:26`.

* drwxr-xr-x 6 ubuntu ubuntu 4096 Dec 28 15:54 ..
* -rw-r--r-- 1 root root 10642 Dec 28 14:26 key_dump.json
* -rw-r--r-- 1 root root 19423 Dec 28 14:26 key_table_dump.json
* drwxr-xr-x 10 root root 4096 Dec 28 14:26 organizations
* drwxr-xr-x 2 root root 4096 Dec 28 14:26 user_acls
* drwxr-xr-x 2 root root 4096 Dec 28 14:26 users

### Zip and Download to Local

You can zip and download the *knife-ec-backup* command to local by running the following command:

```cmd
zip -r directory.zip directory
```

{{< note >}} The above command is one of the option to zip the *knife-ec-backup* command. You can also the command using any other option.
{{< /note >}}
