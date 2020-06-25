+++
title = "Restore"
description = "Procedures for Restoring Chef Automate Data"
date = 2018-03-26T15:27:52-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 80
+++

Restore Chef Automate from a [filesystem backup]({{< ref "restore.md#restore-from-a-filesystem-backup" >}}) or an [Amazon S3 bucket backup]({{< ref "restore.md#restore-from-an-s3-backup" >}}).

## Prerequisites

1. On the restore host, download and unzip the Chef Automate command-line tool:

   ```shell
        curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

1. **Filesystem backups** require access for Chef Automate to a backup directory in the [configured location]({{< ref "backup.md#backup-to-a-filesystem" >}}).
Ensure access for the backup type used:

     1. To restore [a network-attached filesystem backup]({{< ref "backup.md#backup-to-a-filesystem" >}}), mount the shared backup directory to the same mount point configured at the time of the backup.
     1. To restore [a backup directory that is not a network-attached filesystem]({{< ref "backup.md#backup-to-a-filesystem" >}}), copy the backup directory to the configured location at the time of the backup.
     1. For restoring a [single-file backup archive]({{< ref "backup.md#store-a-filesystem-backup-in-a-single-file-archive" >}}), copy your archive to the restore host and extract it to the configured backup directory.

1. If restoring to a host with a different fully qualified domain name (FQDN) than that of the backup host, then create a `patch.toml` file that specifies the new FQDN and provides it at restore time:

    ```toml
         [global.v1]
         fqdn = "<new-fqdn>"

         # To provide a cert and key for the restore host, uncomment and fill
         # these sections.
         # [[global.v1.frontend_tls]]
         # The TLS certificate for the load balancer frontend.
         # cert = """-----BEGIN CERTIFICATE-----
         # <certificate-for-new-fqdn>
         # -----END CERTIFICATE-----
         # """

         # The TLS RSA key for the load balancer frontend.
         # key = """-----BEGIN RSA PRIVATE KEY-----
         # <key-for-new-fqdn>
         # -----END RSA PRIVATE KEY-----
         # """
    ```

## Restore From a Filesystem Backup

Meet the required [prerequisites]({{< ref "restore.md#prerequisites" >}}) before beginning your restore process.

### Restore in an Internet-Connected Environment

If you have [configured the backup directory]({{< relref "backup.md#backup-to-a-filesystem" >}}) to a directory other than the default directory (`/var/opt/chef-automate/backups`), you must supply the backup directory.
Without a backup ID, Chef Automate uses the most recent backup in the backup directory.

```shell
chef-automate backup restore </path/to/backups/>BACKUP_ID [--patch-config </path/to/patch.toml>] [--skip-preflight]
```

Use the `--patch-config` option with a [configuration patch file]({{< relref "backup.md#prerequisites" >}}) to restore to a host with a different FQDN than that of the backup host.
Use the `--skip-preflight` option to restore to a host with a pre-existing Chef Automate installation.

Incorrect directory permissions cause restore from a filesystem backup to fail.
Run the [`fix-repo-permissions` command]({{< ref "cli-chef-automate/#chef-automate-backup-fix-repo-permissions" >}}) to address such issues:

```shell
sudo chef-automate backup fix-repo-permissions <path>
```

### Restore in an Airgapped Environment

To restore a backup of an [airgapped installation]({{< relref "airgapped-installation.md" >}}), you must specify the [Airgap Installation Bundle]({{< relref "airgapped-installation.md#create-an-airgap-installation-bundle" >}}) used by the installation.
If you have [configured the backup directory]({{< relref "backup.md#backup-to-a-filesystem" >}}) to a directory that is not the default `/var/opt/chef-automate/backups`, then you must supply the backup directory.
If you do not provide a backup ID, Chef Automate uses the most recent backup in the backup directory.

```shell
chef-automate backup restore --airgap-bundle </path/to/bundle> </path/to/backups/>BACKUP_ID [--patch-config </path/to/patch.toml>] [--skip-preflight]
```

Use the `--patch-config` option with a [configuration patch file]({{< relref "backup.md#prerequisites" >}}) to restore to a host with a different FQDN than that of the backup host.
Use the `--skip-preflight` option to restore to a host with a pre-existing Chef Automate installation.

Incorrect directory permissions cause restore from a filesystem backups to fail.
Run the [`fix-repo-permissions` command]({{< ref "cli-chef-automate/#chef-automate-backup-fix-repo-permissions" >}}) to address such issues:

```shell
sudo chef-automate backup fix-repo-permissions <path>
```

## Restore From an S3 Backup

Meet the required [prerequisites]({{< ref "restore.md#prerequisites" >}}) before beginning your restore process.

To restore from an S3 bucket backup, run:

```shell
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID [--patch-config </path/to/patch.toml>] [--skip-preflight]
```

Use the `--patch-config` option with a [configuration patch file]({{< relref "backup.md#prerequisites" >}}) to restore to a host with a different FQDN than that of the backup host.
Use the `--skip-preflight` option to restore to a host with a pre-existing Chef Automate installation.

## Troubleshooting

Set the log level to `debug` before re-running a failed restore to output debug info to the Chef Automate log:

```shell
chef-automate debug set-log-level deployment-service debug
```

## References

See the [`chef-automate backup restore` command reference]({{< ref "cli-chef-automate/#chef-automate-backup-restore" >}}).
