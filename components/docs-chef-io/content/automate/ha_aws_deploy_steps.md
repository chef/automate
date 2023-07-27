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

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

## Install Chef Automate HA on AWS

### Prerequisites

-   Virtual Private Cloud (VPC) should be created in AWS before starting. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
-   If you want to use Default VPC we have to create public and private subnet, If subnet are not available. Please refer [this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
-   We need 3 private and 3 public subnet in a vpc (1 subnet for each AZ). As of now we support dedicate subnet for each AZ.
-   We recommend to create a new vpc. And Bastion should be in the same VPC.
-   Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) which have privileges like: `AmazonS3FullAccess`, `AdministratorAccess`. \
     Set these in `~/.aws/credentials` in Bastion Host:

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

-   Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
-   Have SSH Key Pair ready in AWS, so new VM's are created using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
-   We do not support passphrase for Private Key authentication.
-   Preferred key type will be ed25519. Deployment on RHEl 9 & Ubuntu 22.04.x the ed25519 ssh key algorithms is required.
-   Make sure your linux has `sysctl` utility available in all nodes.

{{< warning >}}

-   PLEASE DONOT MODIFY THE WORKSPACE PATH it should always be "/hab/a2_deploy_workspace"
-   We currently don't support AD managed users in nodes. We only support local linux users.
-   If you have configured sudo password for the user, then you need to create an environment variable `sudo_password` and set the password as the value of the variable. Example: `export sudo_password=<password>`. And then run all sudo commands with `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.

{{< /warning >}}

### Deployment

Run the following steps on Bastion Host Machine:

{{< note >}}

-   Make sure that bastion machine is in the same vpc, as mention in `config.toml`, otherwise we need to do [vpc peering](https://docs.aws.amazon.com/vpc/latest/peering/what-is-vpc-peering.html).
-   Use subnet-id instead of CIDR block in `config.toml`, to avoid the subnet conflict. If we use CIDR block, will fail if an consecutive cidr block are not available.
-   If you choose `backup_config` as `s3` then provide the bucket name to field `s3_bucketName`. If `s3_bucketName` exist it is directly use for backup configuration and if it doesn't exist then deployment process will create `s3_bucketName`.
-   If you choose `backup_config` as `efs` then we will create the EFS and mount on all frontend and backend node.
-   If you choose `backup_config` as `" "` (empty), then you have to manually to do the backup configuration, after the deployment complete. But we recommended that to use `backup_config` to be set to `s3` or `efs` at the time of deployment.

{{< /note >}}

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

    {{< note >}} Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever. {{< /note >}}

1. Update Config with relevant data. Click [here](#sample-config) for sample config

    ```bash
    vi config.toml
    ```

    - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
    - Optional `ssh_group_name` make sure given group name is available in all machines, this value will be defaulted to `ssh_user`.
    - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
    - Give `ssh_key_file` path, this should have been download from AWS SSH Key Pair which we want to use to create all the VM's. Thus, we will be able to access all VM's using this.
    - We support only private key authentication.
    - Set `backup_config` to `"efs"` or `"s3"`
    - If `backup_config` is `s3` then uncomment and set the value for following `s3_bucketName` attribute to your bucket name. If the bucket name does not exist, it will be created for you automatically.
    - Set `admin_password` which you can use to access Chef Automate UI for user `admin`.
    - If you don't have a custom FQDN leave `fqdn` as empty for this AWS deployment. By default, AWS Application load balancer will be used as `fqdn`.
    - Set `instance_count` for Chef Automate, Chef Infra Server, Postgresql, OpenSearch.
    - Set AWS Config Details:
        - Set the AWS `profile`. In case you attached the IAM role to the bastion machine, leave this field empty.
        - Set `region`, by default `region` is `"us-east-1"`
        - Set `aws_vpc_id`, which you had created as Prerequisite step. Example: `"vpc12318h"`
        - Set `private_custom_subnets` to the 3 private subnets we created in prerequisites step and set `public_custom_subnets` to the 3 public subnets we created in prerequisites step: example : `["subnet-07e469d218301533","subnet-07e469d218041534","subnet-07e469d283041535"]`
        - Set `ssh_key_pair_name`, this is the SSH Key Pair we created as Prerequisite. This value should be just name of the AWS SSH Key Pair, not having `.pem` extention. The ssh key content should be same as content of `ssh_key_file`.
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

    {{< note >}} Click [here](/automate/ha_cert_deployment) to know more on adding certificates for services during deployment. {{< /note >}}

1. Continue with the provisioning the infra after updating config:

    ```bash
    #Run commands as sudo.
    sudo -- sh -c "
    #Print data in the config
    cat config.toml

    #Run provision command to deploy `automate.aib` with set `config.toml`
    chef-automate provision-infra config.toml --airgap-bundle automate.aib
    "
    ```

1. Once the provisioning is successful, **if you have added custom DNS to your configuration file (`fqdn`), make sure to map the load-balancer FQDN from the output of previous command to your DNS from DNS Provider**. After that continue with the deployment process with following.

    ```bash
    sudo -- sh -c "
    #Run deploy command to deploy `automate.aib` with set `config.toml`
    chef-automate deploy config.toml --airgap-bundle automate.aib

    #After Deployment is done successfully. Check status of Chef Automate HA services
    chef-automate status summary

    #Check Chef Automate HA deployment information, using the following command
    chef-automate info
    "
    ```

1. After the deployment successfully completed. To view the automate UI, run the command `chef-automate info`, you will get the `automate_url`.
  If you want to change the FQDN URL from the loadbalancer URL to some other FQDN URL, then use below template


-   create a file `a2.fqdn.toml`

    ```toml
    [global]
     [global.v1]
      fqdn = "AUTOMATE-DNS-URL-WITHOUT-HTTP"
    ```

-   Run the command to apply the config from bastion

    ```toml
     chef-automate config patch a2.fqdn.toml --automate
    ```

-   create a file `cs.fqdn.toml`

    ```toml
    [global]
     [global.v1]
      fqdn = "AUTOMATE-DNS-URL-WITHOUT-HTTPS"
    [global.v1.external.automate]
      node = "https://AUTOMATE-DNS-URL"
    ```

-   Run the command to apply the config from the bastion

    ```toml
     chef-automate config patch cs.fqdn.toml --chef_server
    ```

{{< note >}} DNS should have entry for `chefautomate.example.com` and `chefinfraserver.example.com` pointing to respective Load Balancers as shown in `chef-automate info` command. {{< /note >}}

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### Sample config

{{< note >}}

-   Assuming 8+1 nodes (1 bastion, 1 for automate UI, 1 for Chef-server, 3 for Postgresql, 3 for Opensearch)

{{< /note >}}

{{< note >}}

-   User only needs to create/setup **the bastion node** with IAM role of Admin access, and s3 bucket access attached to it.

-   Following config will create s3 bucket for backup.

-   To provide multiline certificates use triple quotes like `"""multiline certificate contents"""`

{{< /note >}}

```config

[architecture.aws]
ssh_user = "ec2-user"
ssh_group_name = "ec2-user"
ssh_port = "22"
ssh_key_file = "~/.ssh/my-key.pem"
backup_config = "s3"
s3_bucketName = "My-Bucket-Name"
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "aws"
workspace_path = "/hab/a2_deploy_workspace"
backup_mount = "/mnt/automate_backups"

[automate.config]
admin_password = "MY-AUTOMATE-UI-PASSWORD"
fqdn = ""
instance_count = "2"
config_file = "configs/automate.toml"
enable_custom_certs = false

# Add Automate Load Balancer root-ca
# root_ca = """root_ca_contents"""

# Add Automate node internal public and private keys
# private_key = """private_key_contents"""
# public_key = """public_key_contents"""

[chef_server.config]
instance_count = "2"
enable_custom_certs = false

# Add Chef Server node internal public and private keys
# private_key = """private_key_contents"""
# public_key = """public_key_contents"""

[opensearch.config]
instance_count = "3"
enable_custom_certs = false
# Add OpenSearch root-ca and keys
# root_ca = """root_ca_contents"""
# admin_key = """admin_private_key_contents"""
# admin_cert = """admin_public_key_contents"""
# private_key = """private_key_contents"""
# public_key = """public_key_contents"""

[postgresql.config]
instance_count = "3"
enable_custom_certs = false
# Add Postgresql root-ca and keys
# root_ca = """root_ca_contents"""
# private_key = """private_key_contents"""
# public_key = """public_key_contents"""

[aws.config]
profile = "default"
region = "ap-southeast-2"
aws_vpc_id  = "vpc12318h"
aws_cidr_block_addr  = ""
private_custom_subnets = ["subnet-e556d512", "subnet-e556d513", "subnet-e556d514"]
public_custom_subnets = ["subnet-p556d512", "subnet-p556d513", "subnet-p556d514"]
ssh_key_pair_name = "my-key"
setup_managed_services = false
managed_opensearch_domain_name = ""
managed_opensearch_domain_url = ""
managed_opensearch_username = ""
managed_opensearch_user_password = ""
managed_opensearch_certificate = ""
aws_os_snapshot_role_arn = ""
os_snapshot_user_access_key_id = ""
os_snapshot_user_access_key_secret = ""
managed_rds_instance_url = ""
managed_rds_superuser_username = ""
managed_rds_superuser_password = ""
managed_rds_dbuser_username = ""
managed_rds_dbuser_password = ""
managed_rds_certificate = ""
ami_id = "ami-08d4ac5b634553e16"
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
automate_ebs_volume_size = "200"
automate_ebs_volume_type = "gp3"
lb_access_logs = "false"
X-Contact = ""
X-Dept = ""
X-Project = ""

```

#### Minimum Changes required in sample config

-   Give `ssh_user` which has access to all the machines. Eg: `ubuntu`, `centos`, `ec2-user`
-   Give `ssh_key_file` path, this key should have access to all the Machines or VM's. Eg: `~/.ssh/user-key.pem`
-   Provide `region` Eg: `us-east-1`
-   Provide `aws_vpc_id` Eg: `vpc-0a12*****`
-   Provide `private_custom_subnets` and `public_custom_subnets`
-   Provide `ssh_key_pair_name` Eg: `user-key`
-   Give `ami_id` for the respective region where the infra is been created. Eg: `ami-0bb66b6ba59664870`
-   Provide `certificate ARN` for both automate and Chef server in `automate_lb_certificate_arn` and `chef_server_lb_certificate_arn` respectively.

## Add more nodes In AWS Deployment post deployment

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. It needs the count of the nodes you want to add as as argument when you run the command.
For example,

-   if you want to add 2 nodes to automate, you have to run the:

    ```sh
    chef-automate node add --automate-count 2
    ```

-   If you want to add 3 nodes to chef-server, you have to run the:

    ```sh
    chef-automate node add --chef-server-count 3
    ```

-   If you want to add 1 node to OpenSearch, you have to run the:

    ```sh
    chef-automate node add --opensearch-count 1
    ```

-   If you want to add 2 nodes to PostgreSQL you have to run:

    ```sh
    chef-automate node add --postgresql-count 2
    ```

You can mix and match different services if you want to add nodes across various services.

-   If you want to add 1 node to automate and 2 nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-count 1 --postgresql-count 2
    ```

-   If you want to add 1 node to automate, 2 nodes to chef-server, and 2 nodes to PostgreSQL you have to run:

    ```sh
    chef-automate node add --automate-count 1 --chef-server-count 2 --postgresql-count 2
    ```

Once the command executes, it will add the supplied number of nodes to your automate setup. The changes might take a while.

{{< note >}}

-   If you have patched some external config to any of the existing services then make sure you apply the same on the new nodes as well.
    For example, if you have patched any external configurations like SAML or LDAP, or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, Postgresql, and OpenSearch.
-   The new node will be configured with the certificates which were already configured in your HA setup.

{{< /note >}}

{{< warning >}}
Downgrading the number of instance_count for the backend nodes will result in data loss. We do not recommend downgrading the backend nodes.
{{< /warning >}}

## Remove Single Node From Cluster on AWS Deployment

{{< warning >}}

-   We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](/automate/ha_onprim_deployment_procedure/#replace-node-in-automate-ha-cluster) for the reference.

-   Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.

-   Below process can be done for `chef-server` and `automate`.

{{< /warning >}}

The command requires some arguments so that it can determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the IP address of the node you want to remove as an argument when you run the command.
For example,

-   If you want to remove node of automate, you have to run the:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

-   If you want to remove node of chef-server, you have to run the:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

-   If you want to remove node of OpenSearch, you have to run the:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

-   If you want to remove node of PostgreSQL you have to run:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the supplied node from your HA setup. The changes might take a while.

## Uninstall chef automate HA

{{< danger >}}

-   Running clean up command will remove all AWS resources created by `provision-infra` command
-   Adding `--force` flag will remove storage (Object Storage/ NFS) if it is created by`provision-infra`
    {{< /danger >}}

To uninstall chef automate HA instances after unsuccessfull deployment, run below command in your bastion host.

```bash
    chef-automate cleanup --aws-deployment --force
```

or

```bash
    chef-automate cleanup --aws-deployment
```
