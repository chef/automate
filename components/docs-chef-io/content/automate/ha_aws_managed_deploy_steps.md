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
- If you want to use Default VPC, you have to create a Public and Private Subnet if subnets are unavailable. Please refer [this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
- We need three private and three public subnets in a vpc (1 subnet for each AZ). As of now, we support a dedicated subnet for each AZ.
- We recommend creating a new VPC. And Bastion should be in the same VPC.
- Set up AWS RDS PostgreSQL 13.5-R1 in the same VPC where we have the basion and automate ha node to be created. Click [here](/automate/create_amazon_rds/) to know more.
- Set up AWS OpenSearch 1.3 in the same VPC where we have the basion and automate ha node to be created. Click [here](/automate/create_amazon_opensearch/) to know more.
- For Backup with Managed Service, we have only one option: ' Amazon S3`.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#enabling-opensearch-backup-restore) to know more.
- Preferred key type will be ed25519
- Ensure your Linux has the `sysctl` utility available in all nodes.
- Attach IAM role to the Bastion with `AmazonS3FullAccess`, `AdministratorAccess` privileges or get AWS user credentials with the same privileges. Click [here](/automate/ha_iam_user/) to learn more about creating IAM Users.

Set the AWS user credentials in `~/.aws/credentials` in Bastion Host:

  ```bash
  sudo su -
  ```

  ```bash
  mkdir -p ~/.aws
  echo "[default]" >>  ~/.aws/credentials
  echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
  echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
  echo "region=<AWS-REGION>" >> ~/.aws/credentials
  ```

- Have SSH Key Pair ready in AWS so new VMs are created using that pair.\
  Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
- We do not support passphrases for Private Key authentication.
- Make sure that the bastion machine should be in the same vpc as mentioned in `config.toml`; otherwise, we need to do [vpc peering](https://docs.aws.amazon.com/vpc/latest/peering/what-is-vpc-peering.html).
- Use subnet-id instead of CIDR block in `config.toml`, to avoid the subnet conflict.
- Create the below attributes by following [this document.](/automate/managed_services/#enabling-opensearch-backup-restore)
  - `aws_os_snapshot_role_arn`
  - `os_snapshot_user_access_key_id`
  - `os_snapshot_user_access_key_secret`

  Add this to your `config.toml`

- If you choose `backup_config` as `s3`, provide the bucket name to field `s3_bucketName`. If `s3_bucketName` exists, it is directly used for backup configuration, and if it doesn't exist, then the deployment process will create `s3_bucketName`.
- We recommended using `backup_config` to be set to `s3` at the time of deployment.

{{< warning >}}

- PLEASE DO NOT MODIFY THE WORKSPACE PATH; it should always be "/hab/a2_deploy_workspace"
- We currently don't support AD managed users in nodes. We only support local Linux users.
- If you have configured a sudo password for the user, you must create an environment variable `sudo_password` and set the password as the variable's value. Example: `export sudo_password=<password>`. And then, run all sudo commands with the `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.

{{< /warning >}}

### Run these steps on Bastion Host Machine

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

   {{< note >}}
   Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
   {{< /note >}}
##### Steps to generate config
1. Generate config with relevant data using the below command:

    ```bash
    sudo -- sh -c "
    chef-automate config gen config.toml
    "
    ```
    Click [here](/automate/ha_config_gen) to know more about generating config

    or

    Generate the empty config and populate manually
    ```bash
    sudo -- sh -c "
    chef-automate init-ha-config aws
    "
    ```
    {{< warning spaces=4 >}}
    {{% automate/char-warn %}}
    {{< /warning >}}

##### Steps to provision
1. Continue with the deployment after generating the config:

    ```bash
    #Run commands as sudo.
    sudo -- sh -c "
    #Print data in the config
    cat config.toml
    #Run provision command to deploy `automate.aib` with set `config.toml`
    chef-automate provision-infra config.toml --airgap-bundle automate.aib
    "
    ```

{{< note >}}

Once the provisioning is successful, **if you have added custom DNS to your configuration file (`fqdn`), make sure to map the load-balancer FQDN from the output of the previous command to your DNS from DNS Provider**

{{< /note >}}

#####  Config Verify
1. After successful provision, run verify config command:

    ```bash
    sudo chef-automate verify -c config.toml
    ```
    
    To know more about config verify you can check [Config Verify Doc page](/automate/ha_verification_check/).
    
    Once the verification is succesfully completed, then proceed with deployment, In case of failure please fix the issue and re-run the verify command.


##### Steps to deploy
1. The following command will run the deployment.

    ```bash
    sudo -- sh -c "
   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib
   "
   ```
##### Verify Deployment
1. Once the deployment is successful, we can verify deployment by checking status summary and info
  ```bash
   sudo -- sh -c "
   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status summary

   #Check Chef Automate HA deployment information using the following command
   chef-automate info
   "
  ```

2. After the deployment is completed. To view the automate UI, run the command `chef-automate info`, and you will get the `automate_url`.
  If you want to change the FQDN URL from the loadbalancer URL to some other FQDN URL, then use the below template.

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

After successful deployment, proceed with following...
   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping,  Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstraping.

### Sample Config

{{< note >}} Assuming 8+1 nodes (1 bastion, 1 for automate UI, 1 for Chef-server, Managed RDS Postgresql, and Managed OpenSearch) {{< /note >}}

{{< note >}}

- User only needs to create/set up **the bastion node**, a **user** with IAM role of Admin access and the s3 bucket access attached to it.
- The following config will create an s3 bucket for backup.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`.

{{< /note >}}

```config
[architecture]
  [architecture.aws]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "~/.ssh/my-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "aws"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "s3"
    s3_bucketName = "My-Bucket-Name"
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

#### Minimum Changes required in the sample config

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

## Add more nodes In AWS Deployment post deployment

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. When you run the command, it needs the count of the nodes you want to add as an argument. For example,

- If you want to add two nodes to automate, you have to run the:

    ```sh
    chef-automate node add --automate-count 2
    ```

- If you want to add three nodes to the chef-server, you have to run the:

    ```sh
    chef-automate node add --chef-server-count 3
    ```
