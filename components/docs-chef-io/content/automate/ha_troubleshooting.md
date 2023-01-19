+++
title = "Troubleshooting"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Troubleshooting"
    parent = "automate/deploy_high_availability/troubleshooting"
    identifier = "automate/deploy_high_availability/troubleshooting/ha_troubleshooting.md Troubleshooting"
    weight = 200
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This page explains the frequently encountered issues in Chef Automate High Availability (HA) functionality and the steps to resolve them.

## Issues and Solutions

### Post Automate HA deployment, if chef-server service is in critical state.
- Run the command on Automate HA chef-server node `journalctl --follow --unit chef-automate`
  if getting an 500 internal server error with data-collector endpoint, it means that
  Chef Infra Server not able to communicate to the Chef Automate data-collector endpoint
  
  ssh to the Automate HA Chef Infra Server, and get token and automate-lb-url from the config.
  run `chef-automate config show` go get the config.
  
```cmd
  export endpoint="AUTOMATE LB URL"
  export token="GET_THIS_TOKEN_FROM_CHEF_SERVER_CONFIG"
  curl -H "api-token:$token" https://$endpoint/api/v0/events/data-collector -k
```

To make the service health, make sure that chef server able to curl the data collector endpoint from chef server node.

### Deployment doesn't exit Gracefully
- There are some cases in which deployment doesn't exited successfully.


### Issue: Database Accessed by Other Users

The restore command fails when other users or services access the nodes' databases. This happens when restore service tries to drop the database when some services are still running and are referring to database.

![Database Access Error](/images/automate/ha_faq_access.png)

#### Solution

- Stop the frontend and backend services.

- Perform the following steps on all frontend and backend nodes:

  - SSH into the frontend node and execute the `chef-automate status` command.
  - SSH into the backend node and execute the `hab svc status` command.

### Issue: Cached Artifact not found in Offline Mode

The cached artifact does not exist in offline mode. This issue occurs in an air gap environment when a package tries to pull any dependency components or details from the internet.

![Cache Artifact Error](/images/automate/ha_faq_cache.png)

#### Solution

Use the `--airgap-bundle` option and the `restore` command. Locate the name of the airgap bundle from the path `/var/tmp`. For example, the airgap bundle file name, *frontend-4.x.y.aib*.

##### Command Example

```bash

chef-automate backup restore s3://bucket\_name/path/to/backups/BACKUP\_ID --patch-config </path/to/patch.toml> --skip-preflight --s3-access-key "Access\_Key" --s3-secret-key "Secret\_Key" --airgap-bundle /var/tmp/<airgap-bundle>

```

### Issue: Existing Architecture does not Match with the Requested

The existing architecture does not match the requested one issue occurs when you have made AWS provisioning, and again you are trying to run the `automate-cluster-ctl provision` command.

#### Solution

Execute the following command from the bastion host from any location:

- `sed -i 's/deployment/aws/' /hab/a2\_deploy\_workspace/terraform/.tf\_arch`

- `sed -i 's/architecture "deployment"/architecture "aws"/' /hab/a2\_deploy\_workspace/a2ha.rb`

### Issue: Unable to Determine the Bucket Region

When Chef Automate instances could not locate the S3 bucket, the following error is displayed, *Unable to restore the backup: Listing backups failed: RequestError: send request failed caused by: Get "https://s3.amazonaws.com/a2backup"*

![Bucket Region Error](/images/automate/ha_faq_bucket_region.png)

#### Solution

Ensure that the access key, secret key, and endpoints are correct.

If you are using on-premises S3 for backup and facing issues with restore, attach the `s3-endpoint` with  the `s3 restore` command.

For example:

```bash
chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key" --s3-secret-key "Secret_Key" --s3-endpoint "<URL>"
```

### Issue: HAB Access Error

The *hab* user does not have read, write, or executive privileges on the backup repository.

#### Solution

Execute the following command to grant permission to the user.

```bash
sudo chef-automate backup fix-repo-permissions <path>
```

### Issue: Longer Time in Executing Command ./scripts/credentials set ssl

The `./scripts/credentials set ssl` command stuck and could not locate the HAB license.

#### Solution

Press *ctrl + c*, export HAB license, and execute `./scripts/credentials set ssl` command.

### Issue: Deployment Fails Repeatedly Due to Unhealthy Status

The deployment repeatedly fails due to unhealthy status when you execute the command `./chef-automate deploy config.toml`.

![Deployment Error](/images/automate/ha_faq_deployfail.png)

#### Solution

Follow the steps to fix the above issue:

- SSH into all frontends (Chef Automate HA and Chef Server)

- Remove the */hab* directory from all frontend nodes.

- Remove all the files from the */var/tmp* folder of all frontend nodes.

```bash
rm -rf hab && cd /var/tmp && rm -rf
sudo kill -9 $(sudo lsof -t -i:9631)
sudo kill -9 $(sudo lsof -t -i:9638)
```

- Execute the *terraform destroy* command to remove the deployment.

```bash
for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/;terraform destroy;cd $i;done
```

- Re-run the deployment command: `./chef-automate deploy config.toml`.

### Issue: bootstrap.abb scp Error

While trying to deploy Chef Automate HA multiple times on the same infrastructure, the *bootstrap.abb* file is not created again as a state entry from past deployment blocks the creation.

The possible error looks like as shown below:

```bash
    Error running command 'scp -o StrictHostKeyChecking=no -i /root/.ssh/a2ha-hub cloud-user@<ip>5:/var/tmp/bootstrap.abb bootstrap8e143d7d.abb': exit status 1. Output: scp: /var/tmp/bootstrap.abb: No such file or directory
```

#### Solution

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


