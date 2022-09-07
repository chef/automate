+++
title = "Quick Start Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Quick Start Deployment"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_deployment_quick_start.md Quick Start Deployment"
    weight = 200
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

In this section, we'll discuss the steps to quick start Chef Automate HA on-premise deployment.

## Install Chef Automate HA

### On-Premise Deployment

Follow the steps below to deploy Chef Automate HA on-premise machines or on existing VM's.

#### Prerequisites

- All VM's or Machines are up and running.
- OS Root Volume (/) must be at least 40 GB
- TMP space (/var/tmp) must be at least 5GB
- Separate Hab volume (/hab) provisioned at least 100 GB, for opensearch node `/hab` volume will be more based on the data retention policy.
- A Common user has access to all machines.
- This common user should have sudo privileges.
- This common user uses same SSH Private Key file to access all machines.
- Key-based SSH for the provisioning user for all the machine for HA-Deployment.
- LoadBalancers are setup according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
- DNS is configured to redirect `chefautomate.example.com` to Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, then these records should be added to `/etc/hosts` in all the machines including Bastion:

```bash
sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
```

- If the instance is **RedHat**, set SElinux config `enforcing` to `permissive` in all the nodes.\
  SSH to each node then run:

```bash
sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config
```

#### Run these steps on Bastion Host Machine

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha existing_infra
   "
   ```

2. Update Config with relevant data:

   ```bash
   sudo vi config.toml
   ```

   - Add No. of machines for each Service: Chef Automate, Chef Infra Server, Postgresql, OpenSearch
   - Add IP address of each machine in relevant service section, multiple IP's shoud be in double quotes (`"`) and separated with comma (`,`). Example: `["10.0.0.101","10,0.0.102"]`
      - If we want to use same machine for OpenSearch and Postgresql then provide same IP for both the config fields. Which means overall there will 3 machines or VM's running both OpenSearch and Postgresql. A reduced performance should be expected with this. Minimum 3 VM's or Machines will be used for Both OpenSearch and Postgresql running together on all 3 machines.
      - Also, you can use same machines for Chef Automate and Chef Infra Server. Which means overall there will be 2 machines or VM's running both Chef Automate and Chef Infra Server. A reduced performance should be expected with this. Minimum 2 VM's or Machines will be used by both Chef Automate and Chef Infra Server running together on both 2 machines.
      - Thus, overall minimum machines needed will be 5.
   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this key should have access to all the Machines or VM's.
   - Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VM's. Example: `chefautomate.example.com`
   - Set the `admin_password` to what you want to use to login to Chef Automate, when you open up `chefautomate.example.com` in the Browser, for the username `admin`.

3. Continue with the deployment after updating config:

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

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### AWS Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

#### Prerequisites

- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. \
    Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Deployment

Run the following steps on Bastion Host Machine:

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha aws
   "
   ```

2. Update Config with relevant data.

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
     - Set `profile`, by default `profile` is `"default"`
     - Set `region`, by default `region` is `"us-east-1"`
     - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
     - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
     - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
     - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
     - Set `setup_managed_services` as `false`, As these deployment steps are for Non-Managed Services AWS Deployment. Default value is `false`.
     - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
     - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
     - Set Instance Type for Chef Automate in `automate_server_instance_type`.
     - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
     - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
     - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
     - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
     - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
     - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
     - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
     - Set `opensearch_ebs_volume_iops`, `opensearch_ebs_volume_size` based on your load needs.
     - Set `postgresql_ebs_volume_iops`, `postgresql_ebs_volume_size` based on your load needs.
     - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`, `opensearch_ebs_volume_type`, `postgresql_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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
   "
   ```

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### AWS Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

#### Prerequisites

- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. \
    Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Deployment

Run the following steps on Bastion Host Machine:

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha aws
   "
   ```

2. Update Config with relevant data.

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
     - Set `profile`, by default `profile` is `"default"`
     - Set `region`, by default `region` is `"us-east-1"`
     - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
     - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
     - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
     - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
     - Set `setup_managed_services` as `false`, As these deployment steps are for Non-Managed Services AWS Deployment. Default value is `false`.
     - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
     - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
     - Set Instance Type for Chef Automate in `automate_server_instance_type`.
     - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
     - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
     - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
     - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
     - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
     - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
     - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
     - Set `opensearch_ebs_volume_iops`, `opensearch_ebs_volume_size` based on your load needs.
     - Set `postgresql_ebs_volume_iops`, `postgresql_ebs_volume_size` based on your load needs.
     - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`, `opensearch_ebs_volume_type`, `postgresql_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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
   "
   ```

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

#### Destroy infra

{{< danger >}}
Below section will destroy the infrastructure
{{< /danger >}}

##### To destroy AWS infra created with S3 Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. This command will destroy all resources created while provisioning (excluding S3).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

##### To destroy AWS infra created with EFS Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. Following command will remove EFS from terraform state file, so that `destroy` command will not destroy EFS.

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done
    ```

3. This command will destroy all resources created while provisioning (excluding EFS).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

### AWS Managed Services Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services.

#### Prerequisites

- Setup AWS RDS Postgresql 13.5 ([Ref link](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html))
- Setup AWS OpenSearch 1.2 ([Ref link](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html)).
- For Backup and Restore with Managed Service ([Ref link](/automate/managed_services/#prerequisites)).
- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credetials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. Ref. to [create IAM Users](/automate/ha_iam_user/) \
  Set these in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`\
  Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.\
  Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Run these steps on Bastion Host Machine

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

2. Update Config with relevant data

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
      - Set `profile`, by default `profile` is `"default"`
      - Set `region`, by default `region` is `"us-east-1"`
      - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
      - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
      - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
      - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This Value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
      - Set `setup_managed_services` as `true`, As these deployment steps are for Managed Services AWS Deployment. Default value is `false` which should be changed.
        - Set `managed_opensearch_domain_name`, `managed_opensearch_domain_url`, `managed_opensearch_username`, `managed_opensearch_user_password` from the Managed AWS OpenSearch which you created in the Prerequsite steps.
        - Set `managed_opensearch_domain_url` as the URL without Port No. Example: `["vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"]`
        - For backup and restore configuration Set `managed_opensearch_certificate`,
        `aws_os_snapshot_role_arn`,
        `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret` ([Ref link](/automate/managed_services/#prerequisites)).
        - Set `managed_rds_instance_url` as the URL with Port No. Example: `["database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"]`
        - Set `managed_rds_instance_url`, `managed_rds_superuser_username`, `managed_rds_superuser_password`, `managed_rds_dbuser_username`, `managed_rds_dbuser_password` from the Managed AWS RDS Postgresql which you created in the Prerequsite steps.
      - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
      - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
      - Set Instance Type for Chef Automate in `automate_server_instance_type`.
      - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
      - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
      - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
      - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
      - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
      - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
      - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
      - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### AWS Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

#### Prerequisites

- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. \
    Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Deployment

Run the following steps on Bastion Host Machine:

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha aws
   "
   ```

2. Update Config with relevant data.

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
     - Set `profile`, by default `profile` is `"default"`
     - Set `region`, by default `region` is `"us-east-1"`
     - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
     - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
     - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
     - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
     - Set `setup_managed_services` as `false`, As these deployment steps are for Non-Managed Services AWS Deployment. Default value is `false`.
     - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
     - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
     - Set Instance Type for Chef Automate in `automate_server_instance_type`.
     - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
     - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
     - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
     - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
     - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
     - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
     - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
     - Set `opensearch_ebs_volume_iops`, `opensearch_ebs_volume_size` based on your load needs.
     - Set `postgresql_ebs_volume_iops`, `postgresql_ebs_volume_size` based on your load needs.
     - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`, `opensearch_ebs_volume_type`, `postgresql_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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
   "
   ```

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

#### Destroy infra

{{< danger >}}
Below section will destroy the infrastructure
{{< /danger >}}

##### To destroy AWS infra created with S3 Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. This command will destroy all resources created while provisioning (excluding S3).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

##### To destroy AWS infra created with EFS Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. Following command will remove EFS from terraform state file, so that `destroy` command will not destroy EFS.

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done
    ```

3. This command will destroy all resources created while provisioning (excluding EFS).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

### AWS Managed Services Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services.

#### Prerequisites

- Setup AWS RDS Postgresql 13.5 ([Ref link](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html))
- Setup AWS OpenSearch 1.2 ([Ref link](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html)).
- For Backup and Restore with Managed Service ([Ref link](/automate/managed_services/#prerequisites)).
- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credetials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. Ref. to [create IAM Users](/automate/ha_iam_user/) \
  Set these in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`\
  Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.\
  Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Run these steps on Bastion Host Machine

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

2. Update Config with relevant data

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
      - Set `profile`, by default `profile` is `"default"`
      - Set `region`, by default `region` is `"us-east-1"`
      - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
      - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
      - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
      - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This Value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
      - Set `setup_managed_services` as `true`, As these deployment steps are for Managed Services AWS Deployment. Default value is `false` which should be changed.
        - Set `managed_opensearch_domain_name`, `managed_opensearch_domain_url`, `managed_opensearch_username`, `managed_opensearch_user_password` from the Managed AWS OpenSearch which you created in the Prerequsite steps.
        - Set `managed_opensearch_domain_url` as the URL without Port No. Example: `["vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"]`
        - For backup and restore configuration Set `managed_opensearch_certificate`,
        `aws_os_snapshot_role_arn`,
        `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret` ([Ref link](/automate/managed_services/#prerequisites)).
        - Set `managed_rds_instance_url` as the URL with Port No. Example: `["database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"]`
        - Set `managed_rds_instance_url`, `managed_rds_superuser_username`, `managed_rds_superuser_password`, `managed_rds_dbuser_username`, `managed_rds_dbuser_password` from the Managed AWS RDS Postgresql which you created in the Prerequsite steps.
      - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
      - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
      - Set Instance Type for Chef Automate in `automate_server_instance_type`.
      - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
      - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
      - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
      - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
      - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
      - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
      - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
      - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### AWS Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

#### Prerequisites

- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. \
    Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Deployment

Run the following steps on Bastion Host Machine:

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha aws
   "
   ```

2. Update Config with relevant data.

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
     - Set `profile`, by default `profile` is `"default"`
     - Set `region`, by default `region` is `"us-east-1"`
     - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
     - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
     - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
     - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
     - Set `setup_managed_services` as `false`, As these deployment steps are for Non-Managed Services AWS Deployment. Default value is `false`.
     - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
     - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
     - Set Instance Type for Chef Automate in `automate_server_instance_type`.
     - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
     - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
     - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
     - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
     - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
     - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
     - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
     - Set `opensearch_ebs_volume_iops`, `opensearch_ebs_volume_size` based on your load needs.
     - Set `postgresql_ebs_volume_iops`, `postgresql_ebs_volume_size` based on your load needs.
     - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`, `opensearch_ebs_volume_type`, `postgresql_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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
   "
   ```

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

#### Destroy infra

{{< danger >}}
Below section will destroy the infrastructure
{{< /danger >}}

##### To destroy AWS infra created with S3 Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. This command will destroy all resources created while provisioning (excluding S3).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

##### To destroy AWS infra created with EFS Bucket

To destroy infra after successfull provisioning, run below command in your bastion host in same order.

1. This command will initialise the terraform packages

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. Following command will remove EFS from terraform state file, so that `destroy` command will not destroy EFS.

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done
    ```

3. This command will destroy all resources created while provisioning (excluding EFS).

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

### AWS Managed Services Deployment

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud with Managed AWS Services.

#### Prerequisites

- Setup AWS RDS Postgresql 13.5 ([Ref link](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html))
- Setup AWS OpenSearch 1.2 ([Ref link](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html)).
- For Backup and Restore with Managed Service ([Ref link](/automate/managed_services/#prerequisites)).
- Virtual Private Cloud (VPC) should be created in AWS before starting or use default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credetials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. Ref. to [create IAM Users](/automate/ha_iam_user/) \
  Set these in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`\
  Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VM's are created using that pair.\
  Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

#### Run these steps on Bastion Host Machine

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

2. Update Config with relevant data

   ```bash
   vi config.toml
   ```

   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
   - Set `backup_config` to `"efs"` or `"s3"`
   - If `backup_config` is `s3` then set `s3_bucketName` to a Unique Value.
   - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
   - Don't set `fqdn` for this AWS deployment.
   - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
   - Set AWS Config Details:
      - Set `profile`, by default `profile` is `"default"`
      - Set `region`, by default `region` is `"us-east-1"`
      - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
      - If AWS VPC uses CIDR then set `aws_cidr_block_addr`.
      - If AWS VPC uses Subnet then set `private_custom_subnets` and `public_custom_subnets` Example: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
      - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequsite. This Value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
      - Set `setup_managed_services` as `true`, As these deployment steps are for Managed Services AWS Deployment. Default value is `false` which should be changed.
        - Set `managed_opensearch_domain_name`, `managed_opensearch_domain_url`, `managed_opensearch_username`, `managed_opensearch_user_password` from the Managed AWS OpenSearch which you created in the Prerequsite steps.
        - Set `managed_opensearch_domain_url` as the URL without Port No. Example: `["vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"]`
        - For backup and restore configuration Set `managed_opensearch_certificate`,
        `aws_os_snapshot_role_arn`,
        `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret` ([Ref link](/automate/managed_services/#prerequisites)).
        - Set `managed_rds_instance_url` as the URL with Port No. Example: `["database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"]`
        - Set `managed_rds_instance_url`, `managed_rds_superuser_username`, `managed_rds_superuser_password`, `managed_rds_dbuser_username`, `managed_rds_dbuser_password` from the Managed AWS RDS Postgresql which you created in the Prerequsite steps.
      - Set `ami_id`, this value depends on your AWS Region and the Operating System Image you want to use.
      - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
      - Set Instance Type for Chef Automate in `automate_server_instance_type`.
      - Set Instance Type for Chef Infra Server in `chef_server_instance_type`.
      - Set Instance Type for OpenSearch in `opensearch_server_instance_type`.
      - Set Instance Type for Postgresql in `postgresql_server_instance_type`.
      - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
      - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
      - Set `automate_ebs_volume_iops`, `automate_ebs_volume_size` based on your load needs.
      - Set `chef_ebs_volume_iops`, `chef_ebs_volume_size` based on your load needs.
      - Set `automate_ebs_volume_type`, `chef_ebs_volume_type`. Default value is `"gp3"`. Change this based on your needs.

3. Continue with the deployment after updating config:

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

Note: DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).
