+++
title = "Troubleshooting"

date = 2018-05-15T17:27:57-07:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Troubleshooting"
    parent = "automate"
    identifier = "automate/troubleshooting.md Troubleshooting"
    weight = 100
+++

## Index issues

### Indices with bad default mappings

Status of the root cause fix for this can be found at: https://chef-software.ideas.aha.io/ideas/AUTO-I-91

A complaint in the `journalctl -u chef-automate` output that has to do with bad default index mappings may look like

```bash
ingest-service.default(O): time="2022-03-03T00:32:40Z" level=error msg="Failed initializing elasticsearch" error="Error creating index node-1-run-info with error: elastic: Error 400 (Bad Request): mapper [node_uuid] of different type, current_type [text], merged_type [keyword] [type=illegal_argument_exception]"
```

As a result, ingest-service can never properly start up, which also breaks automate-cs-nginx and automate-cs-oc-erchef processes, as they need to connect to port 10122, where ingest-service would be listening if it were not restarting continuously.

The list of INDEX_NAME given here can be used in the following command sequence to rebuild whichever index has bad mappings

* node-1-run-info
* converge-history-DATE-STAMP
* node-attribute
* node-state-7

First, stop traffic coming in to the Automate system. You can turn off your Chef Server, for example.
If you are running a combined Automate and Chef Server system, use `chef-server-ctl maintenance on`
Choose a method that is comfortable for you.

Then, perform the deletion with the following commands, remembering to substitute the desired INDEX_NAME from the list above.

```bash
chef-automate dev stop-converge
hab svc unload chef/ingest-service
curl -XDELETE localhost:10141/INDEX_NAME
chef-automate dev start-converge
```

Afterwards, the ingest-service will start back up.
If you continue to see mapping errors, it may be best to contact Support to get a better idea what is going on.

## chef-automate CLI Errors

### Error: Unable to make a request to the deployment-service

The `chef-automate` CLI emits this error when the CLI is unable to communicate with a Chef Automate deployment. In particular, when Chef Automate 2 (as distinct from Chef Automate 1) is not deployed, running `chef-automate` CLI commands such as `version` or `status` causes this error.

## File exists (os error 17)

It's possible for the following error to be emitted when deploying Chef Automate:

```bash
DeploymentServiceCallError: A request to the deployment-service failed: Request to configure deployment failed: rpc error: code = Unknown desc = failed to binlink command "chef-automate" in pkg "chef/automate-cli/0.1.0/20181212085335" - hab output: >> Binlinking chef-automate from chef/automate-cli/0.1.0/20181212085335 into /bin
xxx
xxx File exists (os error 17)
xxx
: exit status 1
```

This problem can be fixed by removing the `chef-automate` binary from the `/bin` directory. The binary
should not be placed in the PATH manually as the deployment process will do it.

## Compliance Report Display

If the size of a Compliance Report is over 4MB, then the _Reports_ page (Compliance > Reports) may not display as expected.
[Audit Cookbook 9.4.0 and later](https://github.com/chef-cookbooks/audit) supports some attribute options that trims a report to its smallest size when combined with latest Chef Automate version.
Contact Chef Support to determine the best way to manage your Compliance Report size.

## Low Disk Space

Chef Automate emits a warning when the available disk space on the system drops below 1 GB, for example:

```bash
es-sidecar-service.default(O): time="2018-05-16T00:07:16Z" level=error msg="Disk free below critical threshold" avail_bytes=43368448 host=127.0.0.1 mount="/ (overlay)" threshold_bytes=536870912 total_bytes=31361703936
```

### Recovering from Low Disk Conditions

Chef Automate disables disk writes if available disk space drops below 250 MB and logs a message similar to:

```bash
ingest-service.default(O): time="2018-05-16T00:10:09Z" level=error msg="Message failure" error="rpc error: code = Internal desc = elastic: Error 403 (Forbidden): blocked by: [FORBIDDEN/12/index read-only / allow delete (api)]; [type=cluster_block_exception] elastic: Error 403 (Forbidden): blocked by: [FORBIDDEN/12/index read-only / allow delete (api)]; [type=cluster_block_exception]"
```

After freeing up disk space, you will need to remove the write block on the OpenSearch indices by running:

```bash
curl -X PUT "localhost:10141/_all/_settings" -H 'Content-Type: application/json' -d'
{
  "index.blocks.read_only_allow_delete": null
}
'
```

To confirm that you've successfully removed the blocks, run:

```bash
curl 'localhost:10141/_all/_settings'
```

Verify that the output does not contain `"blocks":{"read_only_allow_delete":"true"}`.

## Uninstalling Chef Automate

The following procedure will remove Chef Automate from your system,
including all data. If you wish to preserve the data, make a backup
before uninstalling.

With the `chef-automate` CLI:

```bash
chef-automate uninstall
```

## Resetting the Admin Password

Use the following command to completely reset a lost, forgotten, or compromised admin password:

```bash
chef-automate iam admin-access restore NEW_PASSWORD
```

This command causes Automate to inspect your A2 IAM resource and initiates a series of steps for applying the new password to the "admin" user--in effect reconstituting the admin--and connecting it with full administrative permissions.

The process Automate follows for resetting the admin password may include: recreating the user record, recreating the "admins" local team, and recreating the default policy that grants access to all resources for the newly reconstituted local admin team.

To see what exactly will happen in your system, pass `--dry-run`:

```bash
chef-automate iam admin-access restore NEW_PASSWORD --dry-run
```

## Issue: Increase in Data collector API failure

### Details

Possible reason of failures in data collector API could be due to change in the use case we are running.

This can be due to the following reasons:

- Increase in frequency of scan
- Number of controls have changed
- Increase in number of nodes

### Possible fixes

- Changing heap size. Heap size should not be more than 70% of the RAM
- Upgrading machine to improve performance

## Issue: Maximum Shards Open

### Details

The max shards are the number of shards that can be patched for running the data lifecycle to avoid overloading of shards.

### Fixes

The error for the shards occurs when the limit of the data injection to the OpenSearch drains. For example, the shards requirement is 1025 whereas the default value of max shards is **1000**. This is a performance issue from OpenSearch which can be fixed by setting the value of max shards per node.

```bash
Validation Failed: 1: this action would add [10] total shards, but this cluster currently has [1997]/[2000] maximum shards open; [type=validation_exception]
```

To set the value of max shards per node, patch the following configuration in the `.toml` file.

```bash
[opensearch.v1.sys.cluster]
max_shards_per_node = 1000
```

Once done, run the chef-automate config patch `</path/to/your-file.toml>` to deploy your change.

## Issue: Knife search limits at 10000 records

### Details

The knife node list and knife node search commands are inconsistent in the number of records they return. By default, knife search returns a maximum of 10,000 records.

### Fixes

This issue occurs because OpenSearch, by default, limits the maximum number of records (or documents) returned in a single query to 10,000. This safeguard prevents large queries from overloading the system. This approach will do that if you try to retrieve more than 10,000 records.

#### Step 1: Increase the max_result_window to retrieve more than **10000** records.

In the case of embedded opensearch:

```bash
curl -XPUT "http://127.0.0.1:10144/chef/_settings" \
    -d '{
          "index": {
            "max_result_window": 50000
          }
        }' \
    -H "Content-Type: application/json"
```

Changes can be verified by doing the following:

```bash
curl http://127.0.0.1:10144/_settings?pretty
```

For external OpenSearch, ensure the `max_result_window` is also increased accordingly.

#### Step 2: Patch the config in Automate

To set the value of tracking total hits in OpenSearch, patch the following configuration in the `.toml` file.

```bash
[erchef.v1.sys.index]
 track_total_hits = true
```

Once done, run the chef-automate config patch `</path/to/your-file.toml>` to deploy your change.
