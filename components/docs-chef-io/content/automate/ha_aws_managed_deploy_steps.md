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

{{< warning >}}
 {{% automate/ha-warn %}}
{{< /warning >}}

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services.

## Install Chef Automate HA on AWS with Managed AWS Services

### Prerequisites

- Virtual Private Cloud (VPC) should be created in AWS before starting. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- If you want to use Default VPC we have to create Public and Private Subnet if subnet are not available. Please refer [this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
- Setup AWS RDS Postgresql 13.5 in the same VPC where we have the basion and automate ha node going to be created. Click [here](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html) to know more.
- Setup AWS OpenSearch 1.3.6 in the same VPC where we have the basion and automate ha node going to be created. Click [here](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html) to know more.
- For Backup with Managed Service we have only option is `Amazon S3`.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#prerequisites) to know more.
- Get AWS credetials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`. Click [here](/automate/ha_iam_user/) to know more on how to create IAM Users.

Set the above prerequisites in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  echo "region=<AWS-REGION>" >> ~/.aws/credentials
  ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`\
  Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.\
  Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
- We do not support passphrase for Private Key authentication.
- Make sure that bastion machine should be in the same vpc as mention in `config.toml`, otherwise we need to do [vpc peering](https://docs.aws.amazon.com/vpc/latest/peering/what-is-vpc-peering.html).
- Use subnet-id instead of CIDR block in `config.toml`, to avoid the subnet conflict.
- If below attribute are not privided in `config.toml`, in that case terrafrom code will create and use it.
  - `aws_os_snapshot_role_arn`
  - `os_snapshot_user_access_key_id`
  - `os_snapshot_user_access_key_secret`
- If you choose `backup_config` as `s3` then provide the bucket name to feild `s3_bucketName`. If `s3_bucketName` exist it is directly use for backup configuration and if it doesn't exist then deployment code wil tries to create `s3_bucketName`.
- We recommended that to use `backup_config` to be set to `s3` at the time of deployment.

{{< warning >}} PLEASE DONOT MODIFY THE WORKSPACE PATH it should always be "/hab/a2_deploy_workspace"
{{< /warning >}}

### Run these steps on Bastion Host Machine

1. Run below commands to download latest Automate CLI and Airgapped Bundle:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Download Chef Automate CLI.
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip \
   | gunzip - > chef-automate && chmod +x chef-automate \
   | cp -f chef-automate /usr/bin/chef-automate

   #Download latest Airgapped Bundle.
   #To download specific version bundle, example version: 4.2.59 then replace latest.aib with 4.2.59.aib
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate.aib

   #Generate init config and then generate init config for AWS infra structure
   chef-automate init-config-ha aws
   "
   ```

   {{< note >}}
   Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
   {{< /note >}}

1. Update Config with relevant data

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`.
   - Give `ssh_port` if your AMI runs on custom **ssh port**. The default value is 22.
   - Give `ssh_key_file` path, downloaded from **AWS SSH Key Pair**, which you want to use to create all the VMs. This will let you access all the VMs.
   - `sudo_password` is only meant to switch to sudo user. If you have configured password for sudo user, please provide it here.
   - We support only private key authentication.
   - Set `backup_config` to `"s3"`. If `backup_config` is `s3`, set `s3_bucketName`.
   - Set `admin_password` to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for the AWS deployment.
   - Set `instance_count` for *Chef Automate*, *Chef Infra Server*, *Postgresql*, *OpenSearch*.
   - Set AWS Config Details:
      - Set `profile`. The default value of `profile` is `"default"`.
      - Set `region`. The  default value of `region` is `"us-east-1"`.
      - Set `aws_vpc_id`, created in the Prerequisites step. Example: `"vpc12318h"`.
      - If AWS VPC uses Subnet, set `private_custom_subnets` and `public_custom_subnets`. For example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`.
      - Set `ssh_key_pair_name`, an SSH Key Pair created as a Prerequisite. The pair value should be the name of the AWS SSH Key Pair without a `.pem` extension. The ssh key content should be the same as the content of `ssh_key_file`.
      - Set `setup_managed_services` as `true`, As these deployment steps are for Managed Services AWS Deployment. The default value is `false`, which should be changed.
        - Set `managed_opensearch_domain_name`, `managed_opensearch_domain_url`, `managed_opensearch_username`, `managed_opensearch_user_password` from the **Managed AWS OpenSearch** created in the Prerequisite steps.
        - Set `managed_opensearch_domain_url` as the URL without Port No. For example: `["vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"]`.
        - For backup and restore configuration set `managed_opensearch_certificate`, `aws_os_snapshot_role_arn`, `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret`. Click [here](/automate/managed_services/#prerequisites) to know more.
        - Set `managed_rds_instance_url` as the URL with Port No. For example: `["database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"]`
        - Set `managed_rds_instance_url`, `managed_rds_superuser_username`, `managed_rds_superuser_password`, `managed_rds_dbuser_username`, `managed_rds_dbuser_password` from the **Managed AWS RDS Postgresql** created in the Prerequisite steps.
      - Set the `ami_id` value, which depends on the AWS Region and the Operating System image you want to use.
      - Use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information on which instance type you will need for your load.
      - Set Instance Type for:
         - Chef Automate in `automate_server_instance_type`.
         - Chef Infra Server in `chef_server_instance_type`.
      - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
      - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
      - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
      - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
      - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

{{< warning >}}
{{% automate/char-warn %}}
{{< /warning >}}

1. Continue with the deployment after updating config:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Print data in the config
   cat config.toml

   #Run provision command to deploy `automate.aib` with set `config.toml`
   chef-automate provision-infra config.toml --airgap-bundle automate.aib

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check status of Chef Automate HA services
   chef-automate status
   
   #Check Chef Automate HA deployment information, using the following command
   chef-automate info
   ```

1. After the deployment successfully completed. To view the automate UI, run the command `chef-automate info`, you will get the `automate_url`.
  If we want to change the FQDN URL from the loadbalancer URL to some other FQDN URL, then use below template
  
- create a file `a2.fqdn.toml`

  ```toml
  [global]
   [global.v1]
    fqdn = "AUTOMATE-DNS-URL-WITHOUT-HTTP"
  ```

- Run the command to apply the config from bastion

  ```toml
   chef-automate config patch a2.fqdn.toml --automate
  ```

- create a file `cs.fqdn.toml`
  
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

{{< note >}} DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command. {{< /note >}}

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).
