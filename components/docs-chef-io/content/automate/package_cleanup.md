+++
title = "Habitat package cleanup"

date = 2025-12-09T12:00:00-00:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Package cleanup"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/package_cleanup.md Package Cleanup"
    weight = 95
+++

The `chef-automate package-cleanup` command identifies and removes unused Chef Habitat packages.
Over time, as Chef Automate is upgraded and services are updated, old package versions accumulate on the system, consuming valuable disk space.
This command provides a safe, automated way to reclaim that space while protecting essential packages and active services.

This command supports both standalone and high availability (HA) deployments.

## When to run a package cleanup

Consider running package cleanup in these scenarios:

- After upgrading Chef Automate to a new version
- When disk space is running low on your Chef Automate server
- As part of regular system maintenance (monthly or quarterly)
- Before creating system backups to reduce backup size
- After testing new features that required additional packages

## Requirements

- Root or sudo access to Chef Automate or the bastion host for HA deployments
- For HA deployments, SSH connectivity from the bastion host to all HA nodes
- Chef Automate must be running
- There must be sufficient disk I/O capacity for the cleanup operation

## Command syntax

The package cleanup command has the following syntax:

```bash
sudo chef-automate package-cleanup [options]
```

### Command options

The following options are available with the `package-cleanup` command:

`--dry-run`
: Show which packages would be deleted without actually deleting them. Displays package count and estimated space savings.

  Default value: `false`

`--verbose`
`-v`
: Enable detailed progress output showing running services, allowlist contents, per-package deletion status, and multi-pass statistics.

  Default value: `false`

`--debug`
`-d`
: Enable debug output (shows the chef-automate version and build info).

  Default value: `false`

`--help`
`-h`
: Display command help and usage information.

## Clean up Habitat packages

To clean up your Habitat packages, follow these steps:

1. Back up your system before cleanup. For more information, see the [standalone backup](/automate/ha_backup_restore/) and [HA backup](/automate/ha_backup_restore/) documentation.

1. Optional: If you want to see how much space this process recovers, get the amount of space the Habitat packages currently occupy:

    ```sh
    df -h /hab/pkgs
    ```

1. Run a dry-run preview to see which packages will be deleted:

    ```bash
    sudo chef-automate package-cleanup --dry-run
    ```

    Use the `--verbose` option for a more detailed output.

    The `dry-run` option displays:

    - The number of currently running services
    - The total number of packages installed
    - The number of packages in the allowlist (protected)
    - The number of unused packages that would be deleted
    - The estimated space to be reclaimed

1. Clean up Habitat packages.

    Once you've reviewed the dry-run output and are satisfied with the results, run the package cleanup:

    ```bash
    sudo chef-automate package-cleanup
    ```

    The command does the following:

    - Shows an analysis of running services and installed packages
    - Deletes unused packages
    - Reports completion with statistics

1. Optional: See how much space the Habitat packages occupy after the cleanup:

    ```sh
    df -h /hab/pkgs
    ```

## Best practices

- Always do a dry-run first:

  ```bash
  # Preview before cleanup
  sudo chef-automate package-cleanup --dry-run

  # Review output carefully, then proceed
  sudo chef-automate package-cleanup
  ```

- Schedule regular cleanups

  ```bash
  # Monthly cleanup (in cron)
  0 2 1 * * /bin/chef-automate package-cleanup
  ```

- Check disk space before and after:

  ```bash
  # Before
  df -h /hab/pkgs

  # Run cleanup
  sudo chef-automate package-cleanup

  # After
  df -h /hab/pkgs
  ```

- Stop unnecessary services before cleanup to maximize space reclamation:

  ```bash
  # Stop old services no longer needed
  sudo hab svc unload chef/old-service

  # Then run cleanup
  sudo chef-automate package-cleanup
  ```

- Create a backup before major cleanups

  ```bash
  # Create backup before cleanup
  sudo chef-automate backup create

  # Then proceed with cleanup
  sudo chef-automate package-cleanup
  ```

## What the cleanup process does

The Habitat package cleanup process performs the following tasks:

1. Gets the list of current running services (`hab svc status`).
1. Builds a list of protected packages.

    This protected packages list includes:

    - All packages currently running as Habitat services.
    - Core Habitat components required for system operations. These are the Habitat binary, Habitat Supervisor, and Habitat launcher.
    - The Automate CLI.
    - All packages that are dependencies of running services, including transitive dependencies.

1. Gets all installed packages with the `hab pkg list --all` command.
1. Identifies all packages not included in the protected packages list.
1. Deletes any unused packages with the `hab pkg uninstall` command.
1. Repeats the cleanup process until no more unused packages are found.

### Multi-pass cleanup process

The package cleanup uses an iterative multi-pass approach to handle complex dependency chains safely.

When packages are deleted, their dependencies may become unused. A single-pass cleanup might miss these newly orphaned packages. The multi-pass approach ensures complete cleanup:

1. **Pass 1**: Removes the first batch of unused packages
1. **Allowlist Rebuild**: Recalculates running services and dependencies
1. **Pass 2**: Identifies newly unused packages (former dependencies of deleted packages)
1. **Iteration**: Continues until no more unused packages are found

### Package cleanup in standalone deployments

In standalone deployments, the command runs locally and cleans up packages on a single node.

### Package cleanup in high availability deployments

In high availability (HA) deployments, the command automatically detects the HA environment and orchestrates cleanup across all nodes using SSH from the bastion host and in the following order:

1. Bastion host
1. Chef Automate frontend nodes
1. Chef Infra Server frontend nodes
1. PostgreSQL backend nodes (self-managed only)
1. OpenSearch backend nodes (self-managed only)

{{< note >}}

- For AWS managed deployments using Amazon RDS or Amazon OpenSearch Service, those node types are automatically skipped.
- Previous versions of Habitat core packages (`hab`, `hab-sup`, `hab-launcher`) are not removed from PostgreSQL and OpenSearch backend nodes.

{{< /note >}}

## Troubleshooting

### Packages aren't removed

If some packages aren't deleted:

1. Get the status of Habitat services:

   ```bash
   sudo hab svc status
   ```

1. Verify that the package exists:

   ```bash
   sudo hab pkg list <ORIGIN>/<PKG_NAME>
   ```

1. Check file locks:

   ```bash
   sudo lsof | grep hab/pkgs
   ```

1. Clean up packages with verbose output:

   ```bash
   sudo chef-automate package-cleanup --verbose
   ```

### Permission errors

If you encounter permission errors:

```bash
# Ensure you're running as root
sudo -i
chef-automate package-cleanup
```

### Habitat service errors

If `hab svc status` fails:

```bash
# Check Habitat supervisor status
sudo systemctl status hab-sup

# Restart if needed
sudo systemctl restart hab-sup
```

### Disk space isn't reclaimed

If disk space doesn't free up as expected:

1. Verify packages are deleted:

   ```bash
   # Count packages before
   sudo hab pkg list --all | wc -l

   # Run cleanup
   sudo chef-automate package-cleanup

   # Count packages after
   sudo hab pkg list --all | wc -l
   ```

1. Check for other space issues:

   ```bash
   # Find large directories
   sudo du -sh /hab/* | sort -h
   ```

1. Check the log files directory:

   ```bash
   # Check log directory size
   sudo du -sh /hab/svc/*/logs
   ```

### High availability-specific troubleshooting

For HA deployments, if the SSH connection between the bastion host and remote nodes fails, verify SSH connectivity from the bastion host.
The `package-cleanup` command is idempotent and can be safely re-run if it fails midway.

## Safety mechanisms

The package cleanup command includes multiple safety mechanisms:

- **Allowlist protection**: Essential packages are never deleted
- **Dependency awareness**: Habitat respects package dependencies during uninstall
- **Dry-run mode**: Preview changes before applying them
- **Individual package handling**: Failed deletions don't stop the entire process
- **Detailed logging**: All actions are logged for audit purposes

## Performance considerations

- Cleanup time scales with the number of packages to delete
- Typical cleanup takes 2-5 minutes for 50-100 packages
- Large cleanups (500+ packages) may take 15-30 minutes
- Disk I/O is the primary performance factor
- No impact on running services during cleanup

## Disk space savings

Typical space savings vary by installation and upgrade history:

- **Small installation** (a few upgrades): 500 MB - 1 GB
- **Medium installation** (multiple upgrades): 1 GB - 3 GB
- **Large installation** (many upgrades, long-running): 3 GB - 10+ GB

Packages are stored in `/hab/pkgs`, with each package consuming:

- Small packages (binaries): 10-50 MB
- Medium packages (services): 50-200 MB
- Large packages (runtimes): 200 MB - 1 GB

## Related commands

- `chef-automate service-versions` - Check versions of running services
- `chef-automate status` - View system status
- `chef-automate upgrade run` - Upgrade Chef Automate
- `hab pkg list --all` - List all installed Habitat packages
- `hab svc status` - Check Habitat service status
- `hab pkg uninstall` - Manually uninstall specific packages

## Additional resources

- [Chef Automate CLI reference]({{< ref "cli.md" >}})
- [Chef Automate upgrade guide]({{< ref "upgrade.md" >}})
- [Chef Automate backup guide]({{< ref "backup.md" >}})
- [Chef Habitat Package Management](https://docs.chef.io/habitat/on_prem_builder/packages/update_packages/)

## Support

If you encounter issues with package cleanup:

- Review the [troubleshooting section](#troubleshooting) above
- Contact [Chef Support](https://www.chef.io/support)
- Visit the [Chef Community Forum](https://discourse.chef.io/)
