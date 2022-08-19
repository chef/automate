+++
title = "AWS Managed Services Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Managed Services Deployment"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_aws_managed_deploy_steps.md AWS Managed Services"
    weight = 220
+++

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services.

## Install Chef Automate HA on AWS with Managed AWS Services

### Prerequisites

- Setup AWS RDS Postgresql 13.5. Click [here](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html) to know more.
- Setup AWS OpenSearch 1.2. Click [here](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html) to know more.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#prerequisites) to know more.
- Create the Virtual Private Cloud (VPC) in AWS before starting or using default. Click [here](/automate/ha_vpc_setup/) to know more about VPC and CIDR creation.
- Get AWS credetials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. Click [here](/automate/ha_iam_user/) to know more on how to create IAM Users.

Set the above prerequisites in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  ```

- Have DNS certificate ready in ACM for 2 DNS entries: `chefautomate.example.com`, `chefinfraserver.example.com`. Click [here](/automate/ha_aws_cert_mngr/) to know more on how to create new DNS Certificate in ACM.
- Have SSH Key Pair ready in AWS, so new VMs are created using that pair. Click [here](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html) to know more about AWS SSH Key Pair creation.

### Bastion Host Machine

1. Before starting, switch to sudo:

   ```bash
   sudo su -
   ```

2. Download Chef Automate CLI:

   ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

3. Download the latest Airgapped Bundle using:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
   ```

   Download specific version bundle using the following command, example version: `4.0.91`:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/4.0.91.aib -o automate-4.0.91.aib
   ```

4. Generate `init` config for AWS infra structure:

   ```bash
   chef-automate init-config-ha aws
   ```

5. Update config with relevant data:

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`.
   - Give `ssh_port` if your AMI runs on custom **ssh port**. The default value is 22.
   - Give `ssh_key_file` path, downloaded from **AWS SSH Key Pair**, which you want to use to create all the VMs. This will let you access all the VMs.
   - Set `backup_config` to `"efs"` or `"s3"`. If `backup_config` is `s3`, set `s3_bucketName` to a **unique value**.
   - Set `admin_password` to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for the AWS deployment.
   - Set `instance_count` for *Chef Automate*, *Chef Infra Server*, *Postgresql*, *OpenSearch*.
   - Set AWS Config Details:
      - Set `profile`. The default value of `profile` is `"default"`.
      - Set `region`. The  default value of `region` is `"us-east-1"`.
      - Set `aws_vpc_id`, created in the Prerequisites step. Example: `"vpc12318h"`.
      - If AWS VPC uses CIDR, set `aws_cidr_block_addr`.
      - If AWS VPC uses Subnet, set `private_custom_subnets` and `public_custom_subnets`. For example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`.
      - Set `ssh_key_pair_name`, an SSH Key Pair created as a Prerequisite. The pair value should be the name of the AWS SSH Key Pair without a `.pem` extension. The ssh key content should be the same as the content of `ssh_key_file`.
      - Set `setup_managed_services` as `true`, As these deployment steps are for Managed Services AWS Deployment. The default value is `false`, which should be changed.
        - Set `managed_opensearch_domain_name`, `managed_opensearch_domain_url`, `managed_opensearch_username`, `managed_opensearch_user_password` from the **Managed AWS OpenSearch** created in the Prerequisite steps.
        - Set `managed_opensearch_domain_url` as the URL without Port No. For example: `["vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"]`.
        - For backup and restore configuration set `managed_opensearch_certificate`, `aws_os_snapshot_role_arn`, `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret`. Click [here](/automate/managed_services/#prerequisites) to know more.
        - Set `managed_rds_instance_url` as the URL with Port No. For example: `["database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"]`
        - Set `managed_rds_instance_url`, `managed_rds_superuser_username`, `managed_rds_superuser_password`, `managed_rds_dbuser_username`, `managed_rds_dbuser_password` from the **Managed AWS RDS Postgresql** created in the Prerequisite steps.
      - Set the `ami_id` value, which depends on the AWS Region and the Operating System Image you want to use.
      - Use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information on which instance type you will need for your load.
      - Set Instance Type for:
         - Chef Automate in `automate_server_instance_type`.
         - Chef Infra Server in `chef_server_instance_type`.
         - OpenSearch in `opensearch_server_instance_type`.
         - Postgresql in `postgresql_server_instance_type`.
      - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
      - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
      - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
      - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
      - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

6. Confirm all the data in the config is correct:

   ```bash
   cat config.toml
   ```

7. Run Provision Command:

   ```bash
   chef-automate provision-infra config.toml --airgap-bundle latest.aib
   ```

   Using a specific version of Chef Automate, for example: `automate-4.0.91.aib`:

   ```bash
   chef-automate provision-infra config.toml --airgap-bundle automate-4.0.91.aib
   ```

8. Deploy `latest.aib` with set `config.toml` using the following command:

   ```bash
   chef-automate deploy config.toml --airgap-bundle latest.aib
   ```

   If deploying a specific version of Chef Automate, for example: Deploy `automate-4.0.91.aib` with set `config.toml`:

   ```bash
   chef-automate deploy config.toml --airgap-bundle automate-4.0.91.aib
   ```

9. After Deployment is done successfully. Check the status of Chef Automate HA services:

   ```bash
   chef-automate status
   ```

10. Check Chef Automate HA deployment information using the following command:

   ```bash
   chef-automate info
   ```

11. Set DNS entries:

   DNS should have an entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in the `chef-automate info` command.

12. Check if Chef Automate UI is accessible from [Domain used for Chef Automate](https://chefautomate.example.com).
