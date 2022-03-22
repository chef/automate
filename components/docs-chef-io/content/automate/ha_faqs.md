+++
title = "FAQs"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "FAQs"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_faq.md FAQs"
    weight = 100
+++

This page explains the frequently encountered issues in Chef Automate High Availability (HA) feature and the steps to resolve them. In addition, this page also lists the health check commands.

## Frequently Asked Questions

### How to check logs while doing backup or restore?

Set *log-level* debug using the command `chef-automate debug set-log-level deployment-service debug` and execute *journalctl* command, `journalctl --follow --unit chef-automate`.

### How to perform infrastructure cleanup for on-premises nodes

Execute the following commands to perform cleanup on the instances and nodes of the deployed Chef Automate HA infrastructure (Automate, Server, three instances of Postgres, three instances of Elastic search)

```bash

rm -rf /hab
cd /var/tmp && rm -f frontend-* && rm -f backend-*
sudo kill -9 $(sudo ps -ef | awk '/[h]ab-sup/{print $2}')

```

Then, Run `rm -rf /hab` on Bastion node.

## Issues and Resolution

### Issue: Database Accessed by Other Users

Restore command fails when the databases of the nodes are accessed by other users or services. This issue happens while the restore service is trying to drop the database when some services are still in a running state and are referring to the database.

![Database Access Error](/images/automate/ha_faq_access.png)

#### Resolution

- Ensure the frontend and backend services are stopped.

- Ensure the *datadog* agent is stopped.

- Perform the following steps on all frontend and backend nodes:

  - SSH into frontend node and execute the command, `chef-automate status`.
  - SSH into backend node and execute the command, `hab svc status`.

### Issue: Cached Artifact not found in Offline Mode

The cached artifact does not exist in offline mode. This issue occurs in an airgap environment, when a package tries to pull any dependency components or details from the internet.

![Cache Artifact Error](/images/automate/ha_faq_cache.png)

#### Resolution

Use `--airgap-bundle` option and the `restore` command. Locate the name of the airgap bundle from the path `/var/tmp`. For example, airgap bundle file name, *frontend-20210908093242.aib*.

##### Command Example

```bash

chef-automate backup restore s3://bucket\_name/path/to/backups/BACKUP\_ID --patch-config </path/to/patch.toml> --skip-preflight --s3-access-key "Access\_Key" --s3-secret-key "Secret\_Key" --airgap-bundle /var/tmp/<airgap-bundle>

```

### Issue: Existing Architecture does not Match with the Requested

The existing architecture does not match the requested one issue occurs when you have made AWS provisioning and again you are trying to run `automate-cluster-ctl provision` command.

#### Resolution

Execute the following command from the bastion host from any location:

- `sed -i 's/deployment/aws/' /hab/a2\_deploy\_workspace/terraform/.tf\_arch`

- `sed -i 's/architecture "deployment"/architecture "aws"/' /hab/a2\_deploy\_workspace/a2ha.rb`

### Issue: Unable to Determine the Bucket Region

When Chef Automate instances could not locate S3 bucket, the following error is displayed, *Unable to restore backup: Listing backups failed: RequestError: send request failed caused by: Get "https://s3.amazonaws.com/a2backup"*

![Bucket Region Error](/images/automate/ha_faq_bucket_region.png)

#### Resolution

Make sure that the access key, secret key, and endpoints are correct.

If you are using on-premises S3 for backup and your are facing issues with restore, attach `s3-endpoint` with `s3 restore` command.

For example:

```bash
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key" --s3-endpoint "<URL>"
```

### Issue: HAB Access Error

The *hab* user does not have read, write, or executive privileges on the backup repository.

#### Resolution

Execute the following command to grant permission to the user,

```bash

sudo chef-automate backup fix-repo-permissions <path>

```

### Issue: Longer Time in Executing Command ./scripts/credentials set ssl

The `./scripts/credentials set ssl` command gets stuck and could not locate the HAB license.

#### Resolution

Press *ctrl + c*, export HAB license, and execute `./scripts/credentials set ssl` command.

### Issue: Deployment Fails Repeatedly Due to Unhealthy Status

The deployment repeatedly fails due to unhealthy status when you execute the command `./chef-automate deploy config.toml`.

![Deployment Error](/images/automate/ha_faq_deployfail.png)

#### Resolution

Do the following steps when you face this issue:

- SSH into all frontends (Chef Automate HA and chef Server)

- Remove */hab* directory from all frontends nodes.

- Remove all the files from the */var/tmp* folder of all frontend nodes.

```bash

rm -rf /hab && cd /var/tmp && rm -rf

sudo kill -9 $(sudo lsof -t -i:9631)

sudo kill -9 $(sudo lsof -t -i:9638)

```

- Execute *terraform destroy* command to remove the deployment.

```bash

for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done

```

- Re-run the deployment command, `./chef-automate deploy config.toml`.

### Issue: bootstrap.abb scp Error

While trying to deploy Chef Automate HA multiple times on the same infrastructure, the *bootstrap.abb* file is not created again as there is a state entry from past deployment that blocks the creation.

The possible error looks like as shown below:

```bash
    Error running command 'scp -o StrictHostKeyChecking=no -i /root/.ssh/a2ha-hub cloud-user@<ip>5:/var/tmp/bootstrap.abb bootstrap8e143d7d.abb': exit status 1. Output: scp: /var/tmp/bootstrap.abb: No such file or directory
```

#### Resolution

1. Log in to the bastion host.

1. Move to directory, `cd /hab/a2_deploy_workspace/terraform/`.

1. Execute the following commands:

```bash

	terraform taint module.bootstrap_automate.null_resource.automate_pre[0]
	terraform taint module.bootstrap_automate.null_resource.automate_pre[1]
	terraform taint module.bootstrap_automate.null_resource.automate_pre[2]
	terraform taint module.bootstrap_automate.null_resource.automate_post[0]
	terraform taint module.bootstrap_automate.null_resource.automate_post[1]
	terraform taint module.bootstrap_automate.null_resource.automate_post[2]

```

## HA Health Check Commands

This section includes commands that you can execute for the Chef Automate cluster part of the Chef Automate High Availability (HA) system. These commands aid you in assessing the health and status of the components part of the HA cluster. It is highly recommended to run these commands on a test cluster before using them in a production environment.

### Log Check Commands

The Chef Automate frontend and backend nodes service logs are available via `journalctl` from each node. You can identify the service by the name in the generated output preceding with the logline.

- Execute the following command, `journalctl --follow --unit hab-sup`, to view the backend logs related to all hab services.

Where the *--unit* displays the logs from the specified unit, and *--follow* means to follow the journal.

- Use the *grep* command to filter the logs related to a specific service. For example, run this command `journalctl --follow --unit hab-sup | grep 'automate-ha-elasticsearch'` to view the log of the habitat component in the Chef Automate frontend node.

- Execute the following command, `journalctl --follow --unit chef-automate`, to view the log of the frontend (chef-automate and chef-server instances) nodes.

- Use the *grep* command to filter the logs for a single service. For example, run this command, `journactl --follow --unit chef-automate | grep ingest.service` to view the ingest logs of the Chef Automate frontend node.

### Health Check Service Commands

- Execute the following command, `chef-automate status`, to SSH the frontend node.

- Execute the following command, `hab svc status`, to SSH the backend node.

- Execute the following command, `hab svc status`, to verify the health of any services on a node.
