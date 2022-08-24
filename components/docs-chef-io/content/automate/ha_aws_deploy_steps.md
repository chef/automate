+++
title = "AWS Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_aws_deploy_steps.md AWS Deployment"
    weight = 210
+++

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

## Install Chef Automate HA on AWS

### Prerequisites

- Create Virtual Private Cloud (VPC) in AWS before using default. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`, `AmazonAPIGatewayAdministrator`. Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`. Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, so new VMs are created using that pair. Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)

### Deployment

Run the following steps on Bastion Host Machine:

1. Switch to `sudo` using the following command:

    ```bash
    sudo su -
    ```

2. Download Chef Automate CLI

    ```bash
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
    ```

3. Download the Airgapped Bundle and the latest Bundle using the following command:

    ```bash
    curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
    ```

4. Download the specific version of the bundle using the following command (`for example : version: 4.0.91`):,

    ```bash
    curl https://packages.chef.io/airgap_bundle/current/automate/4.0.91.aib -o automate-4.0.91.aib
    ```

5. Generate `init` config and generate it for existing infrastructure using the following command:

    ```bash
    chef-automate init-config-ha aws
    ```

6. Update Config with relevant data.

    ```bash
    vi config.toml
    ```

    - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
    - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
    - Give `ssh_key_file` path, and download from AWS SSH Key Pair, which will create all the VMs. Thus, you will be able to access all VMs using this.
    - Set `backup_config` to `"efs"` or `"s3"`. If `backup_config` is set to `s3`, set `s3_bucketName` to a Unique Value.
    - Set `admin_password` to access Chef Automate UI for user `admin`.
    - Don't set `fqdn` for this AWS deployment.
    - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
    - Set AWS Config Details as follows:
        - Set `profile`, by default `profile` is `"default"`.
        - Set `region`, by default `region` is `"us-east-1"`.
        - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`.
        - If AWS VPC uses CIDR, set `aws_cidr_block_addr`.
        - If AWS VPC uses Subnet, set `private_custom_subnets` and `public_custom_subnets` Example: `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`.
        - Set `ssh_key_pair_name`, which was created as Prerequisites. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
        - Set `setup_managed_services` as `false`, as these deployment steps are for Non-Managed Services AWS Deployment. Default value is `false`.
        - Set `ami_id` value which depends on your AWS Region and the Operating System Image you want to use.
        - Please use the [Hardware Requirement Calculator sheet](/calculator/automate_ha_hardware_calculator.xlsx) to get information for which instance type you will need for your load.
        - Set Instance Type for:
            - Chef Automate in `automate_server_instance_type`.
            - Chef Infra Server in `chef_server_instance_type`.
            - OpenSearch in `opensearch_server_instance_type`.
            - Postgresql in `postgresql_server_instance_type`.
        - Set `automate_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
        - Set `chef_server_lb_certificate_arn` with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
        - Set the following based on your load needs:
            - `automate_ebs_volume_iops`
            - `automate_ebs_volume_size`
            - `chef_ebs_volume_iops`
            - `chef_ebs_volume_size`
            - `opensearch_ebs_volume_iops`
            - `opensearch_ebs_volume_size`
            - `postgresql_ebs_volume_iops`
            - `postgresql_ebs_volume_size`
            - `automate_ebs_volume_type`
            - `chef_ebs_volume_type`
            - `opensearch_ebs_volume_type`
            - `postgresql_ebs_volume_type`. Default value is `"gp3"`.

7. Confirm all the data in the config is correct using the following command:

    ```bash
    cat config.toml
    ```

8. Run Provision Command:

    ```bash
    chef-automate provision-infra config.toml --airgap-bundle latest.aib
    ```

    Use the specific version of Chef Automate, example: `automate-4.0.91.aib`

    ```bash
    chef-automate provision-infra config.toml --airgap-bundle automate-4.0.91.aib
    ```

9. Run deploy command and deploy `latest.aib` with set `config.toml`:

    ```bash
    chef-automate deploy config.toml --airgap-bundle latest.aib
    ```

    Deploy the specific version of the Chef Automate, For example: Deploy `automate-4.0.91.aib` with set `config.toml`:

    ```bash
    chef-automate deploy config.toml --airgap-bundle automate-4.0.91.aib
    ```

10. After Deployment is done successfully check the status of Chef Automate HA services using the following command:

    ```bash
    chef-automate status
    ```

11. Check Chef Automate HA deployment information, using the following command:

    ```bash
    chef-automate info
    ```

12. Set DNS entries:

    DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command.

13. Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### Destroy Infra

{{< danger >}} Below section will destroy the infrastructure {{< /danger >}}

#### Destroy AWS Infra created with S3 Bucket

To destroy infra after successfull provisioning, run the below command in your bastion host in same order.

1. The following command will initialize the terraform packages:

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. The following command will destroy all resources created while provisioning(excluding S3):

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```

#### Destroy AWS Infra created with EFS Bucket

To destroy infra after successfull provisioning, run the below command in your bastion host in same order.

1. The following command will initialize the terraform packages:

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform init;cd $i;done
    ```

2. Following command will remove EFS from terraform state file, so that `destroy` command will not destroy EFS.

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform state rm "module.efs[0].aws_efs_file_system.backups";cd $i;done
    ```

3. The following command will destroy all resources created while provisioning(excluding EFS):

    ```bash
    for i in 1;do i=$PWD;cd /hab/a2_deploy_workspace/terraform/destroy/aws/;terraform destroy;cd $i;done
    ```
