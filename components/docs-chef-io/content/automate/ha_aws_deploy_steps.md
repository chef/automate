+++
title = "AWS Deployment with Chef Managed Database"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment with Chef Managed Database"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_aws_deploy_steps.md AWS Deployment with Chef Managed Database"
    weight = 230
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud. Please see the [AWS Deployment Prerequisites](/automate/ha_aws_deployment_prerequisites/) page and move ahead with the following sections of this page.

{{< warning >}}

- Do not modify the workspace path. It should always be `/hab/a2_deploy_workspace`.
- We currently don't support AD managed users in nodes. We only support local Linux users.
- If you have configured a sudo password for the user, you must create an environment variable `sudo_password` and set the password as the variable's value. Example: `export sudo_password=<password>`. And then, run all sudo commands with the `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.
- If SELinux is enabled, deployment with configure it to `permissive` (Usually in case of RHEL SELinux is enabled)

{{< /warning >}}

## Deployment

Run the following steps on Bastion Host Machine:

{{< note >}}

- Ensure the bastion machine is in the same vpc as in `config.toml`. Otherwise, we need to do [vpc peering](https://docs.aws.amazon.com/vpc/latest/peering/what-is-vpc-peering.html).
- Use subnet-id instead of CIDR block in `config.toml`, to avoid the subnet conflict. If we use a CIDR block, will fail if a consecutive cidr block is not available.
- If you choose `backup_config` as `s3`, provide the bucket name to field `s3_bucketName`. If `s3_bucketName` exists, it is directly used for backup configuration, and if it doesn't exist, then the deployment process will create `s3_bucketName`.
- If you choose `backup_config` as `efs`, we will create the EFS and mount it on all frontend and backend nodes.
- If you choose `backup_config` as `" "` (empty), you have to manually do the backup configuration after the deployment. But we recommended that to use `backup_config` be set to `s3` or `efs` at the time of deployment.

{{< /note >}}

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

    {{< note spaces=4 >}}

    Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.

    {{< /note >}}

## Steps to Generate Config

1. Generate a configuration file with relevant data.

    ```bash
    chef-automate config gen config.toml
    ```

    Click [here](/automate/ha_config_gen) to know more about generating config

    {{< note spaces=4 >}}

    You can also generate a configuration file using the `init-config` subcommand.

    chef-automate init-config-ha aws

    {{< /note >}}

## Steps to Provision

1. Continue with the provisioning of the infra after generating the config:

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

## Steps to Deploy

1. The following command will run the deployment. The deployment command will run the verify command internally, to skip a verification process during deploy command use `--skip-verify` flag

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
     chef-automate verify
    ```

1. Get the cluster Info

    ```bash
     chef-automate info
    ```

1. After the deployment is completed. To view the Automate UI, run the command `chef-automate info`, and you will get the `automate_url`. If you want to change the FQDN URL from the load balancer URL to some other FQDN URL, then use the below template.

- Create a file `a2.fqdn.toml`

    ```toml
    [Global]
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

{{< note >}} DNS should have entries for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in the `chef-automate info` command. {{< /note >}}

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

After successful deployment, proceed with the following:

   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping, Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstrapping.

## Sample Config

{{< note >}}

Assuming 10+1 nodes (1 bastion, 2 for Automate UI, 2 for Chef-server, 3 for Postgresql, 3 for OpenSearch)

{{< /note >}}

{{< note >}}

- User only needs to create/set up **the bastion node** with the IAM role of Admin access and S3 bucket access attached.
- The following config will create an S3 bucket for backup.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`

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
    backup_config = "efs"
[automate]
  [automate.config]
    admin_password = "admin-password"
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
    instance_count = "3"
[postgresql]
  [postgresql.config]
    instance_count = "3"
[aws]
  [aws.config]
    profile = "default"  # This should be commented incase if IAM role is attached
    region = "us-east-2"
    aws_vpc_id = "vpc12318h"
    private_custom_subnets = ["subnet-e556d512", "subnet-e556d513", "subnet-e556d514"]
    public_custom_subnets = ["subnet-p556d512", "subnet-p556d513", "subnet-p556d514"]
    ssh_key_pair_name = "my-key"
    ami_id = "ami-0d629fdcxrc7746e4"
    delete_on_termination = true
    automate_server_instance_type = "m5.large"
    chef_server_instance_type = "m5.large"
    opensearch_server_instance_type = "m5.large"
    postgresql_server_instance_type = "m5.large"
    automate_lb_certificate_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
    chef_server_lb_certificate_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
    chef_ebs_volume_iops = "100"
    chef_ebs_volume_size = "200"
    chef_ebs_volume_type = "gp3"
    opensearch_ebs_volume_iops = "100"
    opensearch_ebs_volume_size = "200"
    opensearch_ebs_volume_type = "gp3"
    postgresql_ebs_volume_iops = "100"
    postgresql_ebs_volume_size = "200"
    postgresql_ebs_volume_type = "gp3"
    automate_ebs_volume_iops = "100"
    automate_ebs_volume_size = "210"
    automate_ebs_volume_type = "gp3"
    lb_access_logs = "true"
```

## Uninstall Chef Automate HA

{{< danger >}}

- Running the clean-up command will remove all AWS resources created by the `provision-infra` command
- Adding the `--force` flag will remove storage (Object Storage/ NFS) if it is created by provision-infra`.

{{< /danger >}}

To uninstall Chef Automate HA instances after successful deployment, run the below command in your bastion host. This will delete the AWS resources that are created during provision-infra.

```bash
chef-automate cleanup --aws-deployment --force
```

OR

```bash
chef-automate cleanup --aws-deployment
```

After the `cleanup` command, the following command can be used to remove the deployment workspace in the Bastion machine. This will also remove the logs file inside the workspace.

```bash
hab pkg uninstall chef/automate-ha-deployment
```
