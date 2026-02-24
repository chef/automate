+++
title = "Package Cleanup"

date = 2025-12-09T12:00:00-00:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Package Cleanup"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/package_cleanup.md Package Cleanup"
    weight = 95
+++

The `chef-automate package-cleanup` command helps maintain your Chef Automate installation by identifying and removing unused Habitat packages. Over time, as Chef Automate is upgraded and services are updated, old package versions accumulate on the system, consuming valuable disk space. This command provides a safe, automated way to reclaim that space while protecting essential packages and active services.

This command supports both standalone and high availability (HA) deployments.

## Overview

The package cleanup process works through several stages to ensure system safety:

1. **Discovery**: Identifies all Habitat packages currently installed on the system
2. **Protection**: Builds a whitelist of essential packages, including:
   - All currently running services
   - Dependencies of running services
   - Core Habitat components (hab, hab-sup, and hab-launcher)
   - The Chef Automate Command Line Interface (CLI) itself
3. **Analysis**: Compares installed packages against the whitelist to identify candidates for removal
4. **Cleanup**: Removes unused packages using `hab pkg uninstall`

## When to Use Package Cleanup

Consider running package cleanup in these scenarios:

- After upgrading Chef Automate to a new version
- When disk space is running low on your Chef Automate server
- As part of regular system maintenance (monthly or quarterly)
- Before creating system backups to reduce backup size
- After testing new features that required additional packages

## Deployment Modes

### Standalone Deployments

In standalone deployments, the command runs locally and cleans up packages on a single node.

### High Availability (HA) Deployments

In HA deployments, the command automatically detects the HA environment and orchestrates cleanup across all nodes via SSH from the bastion host in the following order:

1. Bastion node
2. Automate frontend nodes
3. Chef Server frontend nodes
4. PostgreSQL backend nodes (self-managed only)
5. OpenSearch backend nodes (self-managed only)

{{< note >}}
- For AWS managed deployments using Amazon RDS or Amazon OpenSearch Service, those node types are automatically skipped.
- Previous versions of Habitat core packages (`hab`, `hab-sup`, `hab-launcher`) are not removed from PostgreSQL and OpenSearch backend nodes.
{{< /note >}}

## Prerequisites

The following are some prerequisites to be aware of:

- Root or sudo access to the Chef Automate system (or bastion host for HA)
- Chef Automate must be running
- There must be sufficient disk I/O capacity for the cleanup operation
- Recommended: Take a system backup before the cleanup

For HA deployments, additionally:

- SSH connectivity from the bastion to all HA nodes

## Basic Usage

### Preview Mode (Recommended First Step)

Always start with a dry-run to see what packages would be deleted:

```bash
sudo chef-automate package-cleanup --dry-run
```

This command outputs:

- Number of currently running services
- Total number of packages installed
- Number of packages in the whitelist (protected)
- Number of unused packages that would be deleted
- Estimated space to be reclaimed

**CASE 1: Dry-Run - System with unused packages**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...


   Found 45 running service(s)

   Total installed packages: 2847


   Whitelist contains 1247 protected package(s)

   Found 1600 unused package(s) to delete

DRY-RUN MODE

  Would delete 1600 package(s)
  Estimated space to be saved: ~80000 MB


 Dry-run completed - run without --dry-run to delete packages
```

### Actual Cleanup

Once you've reviewed the dry-run output and are satisfied with the results, run the actual cleanup:

```bash
sudo chef-automate package-cleanup
```

The command:

- Displays analysis of running services and installed packages
- Deletes unused packages
- Reports completion with statistics

**CASE 2: Actual Cleanup - Successfully deleted packages**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...


   Found 45 running service(s)

   Total installed packages: 2847


   Whitelist contains 1247 protected package(s)

   Found 1600 unused package(s) to delete


 Starting cleanup...




 Cleanup completed successfully!
  Removed 1600 package(s)
  Estimated space saved: ~80000 MB
```

**CASE 3: Running cleanup when system is already clean**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...


   Found 33 running service(s)

   Total installed packages: 148


   Whitelist contains 148 protected package(s)

   Found 0 unused package(s) to delete


 Starting cleanup...




 Cleanup completed successfully!
  Removed 0 package(s)
```

### Verbose Mode

For detailed progress information, add the `--verbose` flag to any command.

#### Dry-Run with Verbose

```bash
sudo chef-automate package-cleanup --dry-run --verbose
```

Verbose mode provides:

- Step-by-step execution progress
- A complete list of running services and their package identifiers
- All packages in the whitelist (protected packages)
- Detailed statistical breakdown

**CASE 4: Dry-Run with Verbose - System with unused packages**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...

Step 1: Fetching running Habitat services (hab svc status)...
   Found 45 running service(s)

Running services:
  - chef/automate-gateway/4.10.20/20231101120000
  - chef/automate-load-balancer/4.10.20/20231101120000
  - chef/compliance-service/4.10.20/20231101120000
  ... (42 more services)

Step 2: Building whitelist (services + dependencies + essential)...
Step 3: Fetching all installed packages (hab pkg list --all)...
   Total installed packages: 2847


   Whitelist contains 1247 protected package(s)

Whitelisted packages:
  - chef/automate-gateway/4.10.20/20231101120000
  - core/glibc/2.27/20200612213729
  - core/hab/1.6.0/20200612213729
  ... (1244 more packages)

   Found 1600 unused package(s) to delete

DRY-RUN MODE

  Total installed packages:    2847
  Running services:            45
  Whitelist protected:         1247
  Unused candidates found:     1600
  -> 1600 unused package(s) would be deleted
  Estimated space to be saved: ~80000 MB


 Dry-run completed - run without --dry-run to delete packages
```

**CASE 5: Dry-Run with Verbose - System already clean**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...

Step 1: Fetching running Habitat services (hab svc status)...
   Found 33 running service(s)

Running services:
  - chef/automate-gateway/4.10.20/20231101120000
  - core/postgresql13/13.8/20220913145821
  ... (31 more services)

Step 2: Building whitelist (services + dependencies + essential)...
Step 3: Fetching all installed packages (hab pkg list --all)...
   Total installed packages: 148


   Whitelist contains 148 protected package(s)

Whitelisted packages:
  - chef/automate-gateway/4.10.20/20231101120000
  - core/glibc/2.27/20200612213729
  ... (146 more packages)

   Found 0 unused package(s) to delete

DRY-RUN MODE

  Total installed packages:    148
  Running services:            33
  Whitelist protected:         148
  Unused candidates found:     0


 Dry-run completed - run without --dry-run to delete packages
```

#### Actual Cleanup with Verbose

```bash
sudo chef-automate package-cleanup --verbose
```

Verbose mode during actual cleanup shows:

- Step-by-step execution progress
- A list of running services with identifiers
- Complete whitelist contents
- Multi-pass statistics (pass number and packages to delete per pass)
- A final summary with the total passes and packages removed

**CASE 6: Actual Cleanup with Verbose - Multi-pass deletion**

```
 Chef Automate Package Cleanup
  Analyzing installed Habitat packages...

Step 1: Fetching running Habitat services (hab svc status)...
   Found 45 running service(s)

Running services:
  - chef/automate-gateway/4.10.20/20231101120000
  - chef/automate-load-balancer/4.10.20/20231101120000
  ... (43 more services)

Step 2: Building whitelist (services + dependencies + essential)...
Step 3: Fetching all installed packages (hab pkg list --all)...
   Total installed packages: 2847


   Whitelist contains 1247 protected package(s)

Whitelisted packages:
  - chef/automate-gateway/4.10.20/20231101120000
  - core/glibc/2.27/20200612213729
  ... (1245 more packages)

   Found 1600 unused package(s) to delete


 Starting automated cleanup process...
  The cleanup will run in multiple passes until all unused packages are removed.


 Starting cleanup...

=== Cleanup Pass #1 ===
Rebuilding whitelist...
Current installed: 2847 packages
Whitelist size: 1247 packages
To delete in this pass: 1345 packages
  Attempting to delete 1345 candidate(s)...

Packages to delete in this pass:
  - chef/automate-gateway/4.9.0/20230101120000
  - chef/automate-load-balancer/4.9.0/20230101120000
  ... (1343 more packages)

[Per-package deletion progress...]

=== Cleanup Pass #2 ===
Rebuilding whitelist...
Current installed: 1502 packages
Whitelist size: 1247 packages
To delete in this pass: 255 packages
  Attempting to delete 255 candidate(s)...

Packages to delete in this pass:
  - core/old-dep/1.0.0/20200101000000
  ... (254 more packages)

[Per-package deletion progress...]

   Cleanup complete after 2 pass(es)!

 Cleanup completed successfully!
  Removed 1600 package(s)
  Estimated space saved: ~80000 MB
```
* Understanding which packages are being removed in each pass
* Auditing the cleanup process for compliance
* Debugging whitelist protection logic

## High Availability (HA) Usage

In HA deployments, run the command from the bastion host:

```bash
# Preview cleanup across all HA nodes
sudo chef-automate package-cleanup --dry-run

# Execute cleanup across all HA nodes
sudo chef-automate package-cleanup
```

Each node shows its own cleanup summary. Add `--verbose` for detailed output per node.

## Command Options

| Flag | Shorthand | Description | Default |
|------|-----------|-------------|---------|
| `--dry-run` | | Show which packages would be deleted without actually deleting them. Displays package count and estimated space savings. | `false` |
| `--verbose` | `-v` | Enable detailed progress output showing running services, whitelist contents, per-package deletion status, and multi-pass statistics. | `false` |
| `--debug` | `-d` | Enable debug output (shows the chef-automate version and build info). | `false` |
| `--help` | `-h` | Display command help and usage information. | - |

### Using Verbose Mode

For detailed progress tracking and transparency:

```bash
sudo chef-automate package-cleanup --verbose
```

Verbose mode provides:

- Step-by-step execution stages
- A complete list of running services with package identifiers
- All whitelisted/protected packages
- Per-package deletion progress
- Pass-by-pass cleanup statistics
- Detailed final summary

## Protected Packages

The following package categories are automatically protected from deletion:

### Running Services

All packages currently running as Habitat services are protected. To see your running services:

```bash
sudo hab svc status
```

### Essential Habitat Packages

Core Habitat components required for system operation:

- `*/hab/*/*` - Habitat binary
- `*/hab-sup/*/*` - Habitat Supervisor
- `*/hab-launcher/*/*` - Habitat Launcher

### Chef Automate CLI

The CLI tool itself is protected using wildcard pattern:

- `*/automate-cli/*/*` - All versions of the CLI from any origin

### Service Dependencies

All packages that are dependencies of running services, including transitive dependencies.

## Understanding the Whitelist

The whitelist is built dynamically each time you run the command:

1. **Query Running Services**: Uses `hab svc status` to find all active services
2. **Resolve Dependencies**: For each running service, queries its dependency tree using `hab pkg dependencies --transitive`
3. **Add Essentials**: Includes core Habitat packages and the CLI using wildcard patterns
4. **Build Protection Set**: Creates a comprehensive list of packages that must not be deleted

## Multi-Pass Cleanup Process

The package cleanup uses an iterative multi-pass approach to handle complex dependency chains safely.

### Why Multi-Pass?

When packages are deleted, their dependencies may become unused. A single-pass cleanup might miss these newly orphaned packages. The multi-pass approach ensures complete cleanup:

1. **Pass 1**: Removes the first batch of unused packages
2. **Whitelist Rebuild**: Recalculates running services and dependencies
3. **Pass 2**: Identifies newly unused packages (former dependencies of deleted packages)
4. **Iteration**: Continues until no more unused packages are found

### Algorithm

```
WHILE unused packages exist:
  1. Get current running services (hab svc status)
  2. Build whitelist: services + dependencies + essential packages
  3. Get all installed packages (hab pkg list --all)
  4. Identify packages NOT in whitelist
  5. IF no unused packages found THEN exit (cleanup complete)
  6. Delete batch of unused packages
  7. Track successes and failures
  8. Increment pass counter
END WHILE
```

### Example Multi-Pass Scenario

**Initial State**: 2847 packages installed

**Pass 1**:

- Running services: 45
- Whitelist: 1247 protected packages
- To delete: 1345 packages
- After deletion: 1502 packages remain

**Pass 2**:

- Whitelist rebuilds with current state
- Some dependencies of deleted packages are now unused
- To delete: 255 newly identified unused packages
- After deletion: 1247 packages remain (only protected packages)

**Completion**: No more unused packages found after 2 passes

## Best Practices

### Always Use Dry-Run First

```bash
# Preview before cleanup
sudo chef-automate package-cleanup --dry-run

# Review output carefully, then proceed
sudo chef-automate package-cleanup
```

### Schedule Regular Cleanups

Add to your maintenance schedule:

```bash
# Monthly cleanup (in cron)
0 2 1 * * /bin/chef-automate package-cleanup
```

### Monitor Disk Space

Check disk space before and after:

```bash
# Before
df -h /hab/pkgs

# Run cleanup
sudo chef-automate package-cleanup

# After
df -h /hab/pkgs
```

### Combine with Service Management

Stop unnecessary services before cleanup to maximize space reclamation:

```bash
# Stop old services no longer needed
sudo hab svc unload chef/old-service

# Then run cleanup
sudo chef-automate package-cleanup
```

### Backup Before Major Cleanups

Always take a backup:

```bash
# Create backup before cleanup
sudo chef-automate backup create

# Then proceed with cleanup
sudo chef-automate package-cleanup
```

## Troubleshooting

### Package Deletion Failures

If some packages fail to delete:

1. **Check if the package is in use**:

   ```bash
   sudo hab svc status
   ```

2. **Verify that the package exists**:

   ```bash
   sudo hab pkg list <origin>/<name>
   ```

3. **Check file locks**:

   ```bash
   sudo lsof | grep hab/pkgs
   ```

4. **Review verbose output**:

   ```bash
   sudo chef-automate package-cleanup --verbose
   ```

### Permission Errors

If you encounter permission errors:

```bash
# Ensure you're running as root
sudo -i
chef-automate package-cleanup
```

### Habitat Service Errors

If `hab svc status` fails:

```bash
# Check Habitat supervisor status
sudo systemctl status hab-sup

# Restart if needed
sudo systemctl restart hab-sup
```

### Disk Space Not Reclaimed

If disk space doesn't free up as expected:

1. **Verify deletions**:

   ```bash
   # Count packages before
   sudo hab pkg list --all | wc -l
   
   # Run cleanup
   sudo chef-automate package-cleanup
   
   # Count packages after
   sudo hab pkg list --all | wc -l
   ```

2. **Check for other space issues**:

   ```bash
   # Find large directories
   sudo du -sh /hab/* | sort -h
   ```

3. **Consider log files**:

   ```bash
   # Check log directory size
   sudo du -sh /hab/svc/*/logs
   ```

### HA-Specific Troubleshooting

For HA deployments, if SSH connection fails to remote nodes, verify SSH connectivity from the bastion host. The command is idempotent and can be safely re-run if it fails midway.

## Safety Mechanisms

The package cleanup command includes multiple safety mechanisms:

- **Whitelist protection**: Essential packages are never deleted
- **Dependency awareness**: Habitat respects package dependencies during uninstall
- **Dry-run mode**: Preview changes before applying them
- **Individual package handling**: Failed deletions don't stop the entire process
- **Detailed logging**: All actions are logged for audit purposes

## Performance Considerations

- Cleanup time scales with the number of packages to delete
- Typical cleanup takes 2-5 minutes for 50-100 packages
- Large cleanups (500+ packages) may take 15-30 minutes
- Disk I/O is the primary performance factor
- No impact on running services during cleanup

## Disk Space Reclamation

Typical space savings vary by installation and upgrade history:

- **Small installation** (a few upgrades): 500 MB - 1 GB
- **Medium installation** (multiple upgrades): 1 GB - 3 GB
- **Large installation** (many upgrades, long-running): 3 GB - 10+ GB

Packages are stored in `/hab/pkgs`, with each package consuming:

- Small packages (binaries): 10-50 MB
- Medium packages (services): 50-200 MB
- Large packages (runtimes): 200 MB - 1 GB

## Related Commands

- `chef-automate service-versions` - Check versions of running services
- `chef-automate status` - View system status
- `chef-automate upgrade run` - Upgrade Chef Automate
- `hab pkg list --all` - List all installed Habitat packages
- `hab svc status` - Check Habitat service status
- `hab pkg uninstall` - Manually uninstall specific packages

## Additional Resources

- [Chef Automate CLI Reference]({{< ref "cli.md" >}})
- [Chef Automate Upgrade Guide]({{< ref "upgrade.md" >}})
- [Chef Automate Backup Guide]({{< ref "backup.md" >}})
- [Habitat Package Management](https://www.habitat.sh/docs/using-habitat/#manage-packages)

## Support

If you encounter issues with package cleanup:

1. Review the [troubleshooting section](#troubleshooting) above.
2. Check the [Chef Automate documentation]({{< ref "_index.md" >}}).
3. Contact [Chef Support](https://www.chef.io/support).
4. Visit the [Chef Community Forum](https://discourse.chef.io/).
