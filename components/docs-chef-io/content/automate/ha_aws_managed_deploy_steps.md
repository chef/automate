+++
title = "AWS Deployment with AWS Managed Database"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment with AWS Managed Database"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_aws_managed_deploy_steps.md AWS Deployment with AWS Managed Database"
    weight = 240
+++

{{< warning >}}
 {{% automate/ha-warn %}}
{{< /warning >}}

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services. Please see the [AWS Deployment Prerequisites](/automate/ha_aws_deployment_prerequisites/) page and move ahead with the following sections of this page.

{{< warning >}}

- Do not modify the workspace path. It should always be `/hab/a2_deploy_workspace`
- We currently don't support AD managed users in nodes. We only support local Linux users.
- If you have configured a sudo password for the user, you must create an environment variable `sudo_password` and set the password as the variable's value. Example: `export sudo_password=<password>`. And then, run all sudo commands with the `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.
- If SELinux is enabled, deployment with configure it to `permissive` (Usually in case of RHEL SELinux is enabled)

{{< /warning >}}

## Run these steps on Bastion Host Machine

1. Run the below commands to download the latest Automate CLI and Airgapped Bundle:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Download Chef Automate CLI.
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip \
   | gunzip - > chef-automate && chmod +x chef-automate \
   | cp -f chef-automate /usr/bin/chef-automate
   #Download the latest Airgapped Bundle.
   #To download specific version bundle, example version: 4.2.59 then replace latest.aib with 4.2.59.aib
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate.aib
   "
   ```

   {{< note spaces=3 >}}

   Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.

   {{< /note >}}

## Steps to Generate Config

1. Generate config with relevant data using the below command:

    ```bash
    chef-automate config gen config.toml
    ```

    Click [here](/automate/ha_config_gen) to know more about generating config

    {{< note spaces=4 >}}

    You can also generate a configuration file using the `init-config` subcommand. The command is as shown below:

    chef-automate init-config-ha aws

    {{< /note >}}

    {{< warning spaces=4 >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

## Steps to Provision

1. Continue with the deployment after generating the config:

    ```bash
    chef-automate provision-infra config.toml --airgap-bundle automate.aib
    ```

{{< note >}}

Once the provisioning is successful, **if you have added custom DNS to your configuration file (`fqdn`), make sure to map the load-balancer FQDN from the output of the previous command to your DNS from DNS Provider**

{{< /note >}}

## Config Verify

1. After successful provision, run verify config command:

    ```bash
    sudo chef-automate verify -c config.toml
    ```

    To know more about config verify, you can check [Config Verify Doc page](/automate/ha_verification_check/).

    Once the verification is successfully completed, then proceed with deployment, In case of failure, please fix the issue and re-run the verify command.

## Steps to deploy

1. The following command will run the deployment. The deploy command will first run the verify command internally, to skip verification process during deploy command use `--skip-verify` flag

    ```bash
     chef-automate deploy config.toml --airgap-bundle automate.aib
    ```

   To skip verification in the deployment command, use `--skip-verify` flag
    ```bash
     chef-automate deploy config.toml --airgap-bundle automate.aib --skip-verify
    ```

## Verify Deployment

1. Once the deployment is successful, Get the consolidated status of the cluster

    ```bash
     chef-automate status summary
    ```

1.  Get the service status from each node

    ```bash
     chef-automate status
    ```

1. Post Deployment, you can run the verification command

    ```bash
     chef-automate verfiy
    ```

1. Get the cluster Info

    ```bash
     chef-automate info
    ```

1. After the deployment is completed. To view the Automate UI, run the command `chef-automate info`, and you will get the `automate_url`.
  If you want to change the FQDN URL from the load balancer URL to some other FQDN URL, then use the below template.

    - Create a file `a2.fqdn.toml`

    ```toml
    [global]
      [global.v1]
        fqdn = "AUTOMATE-DNS-URL-WITHOUT-HTTP"
    ```

    - Run the command to apply the config from the bastion

    ```toml
    chef-automate config patch a2.fqdn.toml --automate
    ```

    - Create a file `cs.fqdn.toml`

    ```toml
    [global]
      [global.v1]
        fqdn = "AUTOMATE-DNS-URL-WITHOUT-HTTPS"
      [global.v1.external.automate]
        node = "https://AUTOMATE-DNS-URL"
    ```

    - Run the command to apply the config from the bastion

    ```toml
    chef-automate config patch cs.fqdn.toml --chef_server
    ```

{{< note >}}

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`, Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/).
- DNS should have entries for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in the `chef-automate info` command

{{< /note >}}

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

After successful deployment, proceed with the following:

   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping, Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstrapping.

## Sample Config

{{< note >}}

Assuming 8+1 nodes (1 bastion, 1 for Chef Automate UI, 1 for Chef Infra Server, Managed RDS Postgresql, and Managed OpenSearch) 

{{< /note >}}

{{< note >}}

- User only needs to create/set up **the bastion node**, a **user** with IAM role of Admin access and the S3 bucket access attached to it.
- The following config will create an S3 bucket for backup.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`.

{{< /note >}}

```toml
[architecture]
  [architecture.aws]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/KEY_FILENAME.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "aws"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "s3"
    s3_bucketName = "BUCKET_NAME"
[automate]
  [automate.config]
    admin_password = "test@343423"
    fqdn = "chefautomate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"
[chef_server]
  [chef_server.config]
    fqdn = "chefserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"
[opensearch]
  [opensearch.config]
    instance_count = "0"
[postgresql]
  [postgresql.config]
    instance_count = "0"
[aws]
  [aws.config]
    profile = "default"   # This should be commented incase if IAM role is attached
    region = "us-east-2"
    aws_vpc_id = "vpc12318h"
    private_custom_subnets = ["subnet-e556d512", "subnet-e556d513", "subnet-e556d514"]
    public_custom_subnets = ["subnet-p556d512", "subnet-p556d513", "subnet-p556d514"]
    ssh_key_pair_name = "my-key"
    setup_managed_services = true
    managed_opensearch_domain_name = "automate-ha"
    managed_opensearch_domain_url = "vpc-automate-ha-a6uhtsu.ap-southeast-2.es.amazonaws.com"
    managed_opensearch_username = "MY-USER-NAME"
    managed_opensearch_user_password = "MY-OPENSEARCH-PASSWORD"
    aws_os_snapshot_role_arn = "......."
    os_snapshot_user_access_key_id = "......."
    os_snapshot_user_access_key_secret = "......."
    managed_rds_instance_url = "database-1.jux.us-east-2.rds.amazonaws.com:5432"
    managed_rds_superuser_username = "MY-POSTGRES-SUPER-USER-NAME"
    managed_rds_superuser_password = "MY-POSTGRES-PASSWORD"
    managed_rds_dbuser_username = "MY-DB-USERNAME"
    managed_rds_dbuser_password = "MY-DB-PASSWORD"
    ami_id = "ami-08d4ac5b634553e16"
    automate_server_instance_type = "m5.large"
    chef_server_instance_type = "m5.large"
    automate_lb_certificate_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
    chef_server_lb_certificate_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
    chef_ebs_volume_iops = "100"
    chef_ebs_volume_size = "200"
    chef_ebs_volume_type = "gp3"
    automate_ebs_volume_iops = "100"
    automate_ebs_volume_size = "200"
    automate_ebs_volume_type = "gp3"
    lb_access_logs = "true"
```

## Minimum Changes required in the Sample Config

- Provide `ssh_user` which has access to all the machines. E.g., `ec2-user`
- Provide a `ssh_key_file` path; this key should have access to all the Machines or VMs. E.g.: `~/.ssh/user-key.pem`.
- Provide `region` Eg: `ap-southeast-2`.
- Provide `aws_vpc_id` Eg: `vpc-0a12*****`.
- Provide `private_custom_subnets` and `public_custom_subnets`.
- Provide `ssh_key_pair_name` Eg: `user-key`.
- Provide `setup_managed_services` Eg: `true`.
- Provide `managed_opensearch_domain_name`,`managed_opensearch_domain_url`,`managed_opensearch_username`,`managed_opensearch_user_password`.
- Provide `managed_rds_instance_url`,`managed_rds_superuser_username`,`managed_rds_superuser_password`,`managed_rds_dbuser_username`,`managed_rds_dbuser_password`.
- Provide `ami_id` for the region where the infra is created. Eg: `ami-0bb66b6ba59664870`.
- Provide `certificate ARN` for both automate and Chef servers in `automate_lb_certificate_arn` and `chef_server_lb_certificate_arn`, respectively.

## Uninstall Chef Automate HA

{{< danger >}}

The `cleanup` command will remove all AWS resources created by the `provision-infra` command

Adding the `--force` flag will remove object storage if it was created with the `provision-infra` command.

{{< /danger >}}

To uninstall Chef Automate HA instances after successful deployment, run the below command in your bastion host. This will delete the AWS resources that are created during provision-infra.

```bash
chef-automate cleanup --aws-deployment --force
```

OR

```bash
chef-automate cleanup --aws-deployment
```

Following the `cleanup` command the following command can be used to remove the deployment workspace in the Bastion machine. This will also remove the logs file inside the workspace.

```bash
hab pkg uninstall chef/automate-ha-deployment
```
