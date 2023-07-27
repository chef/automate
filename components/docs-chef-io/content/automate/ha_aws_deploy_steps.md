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
-   Preferred key type will be ed25519
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
    "
    ```

    {{< note >}} Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever. {{< /note >}}

1. Generate config with relevant data using below command 
    ```bash
        chef-automate config gen config.toml
    ```
    - Select `Chef Automate HA` as topology
    ```bash
    Choose Topology:
        Chef Automate HA
    ```
    - select deployment type as `AWS`
    ```bash
    Choose type of Deployment Type:
       On-Premise
     > AWS
    ```
    - provide  `ssh user name`, `ssh group name`, `ssh port no`, `ssh key file path`
    - if you have own certificates for Automate, ChefServer, OpenSearch and Postgresql, then select `yes` and provide relevant certificates. Click [here](/automate/ha_cert_deployment) to know more on adding certificates for services during deployment.
    ```bash
        Will you use custom certs for any service like Automate, Chef Infra Server, PostgreSQL, OpenSearch:
        > no
          yes
    ```
    - provide `AWS profile name` skip this if IAM role configured on bashtion host
    - filter and select AWS region from list
    - give AWS VPC ID created in the Prerequisites step. Example: `"vpc12318h"`
    - to create subnets we have two options as CIDR Block and subnets ids, select  `yes` for CIDR and `no` for subnet ids, and provide subnet ids created in the Prerequisites step. Example `subnet-07e469d218301533`, subnets should be created under same VPC provided above, we need three private subnets, if you want to keep loadbalancer on public IP then we need three public subnets as well, recomended is to use subnet ids.
    ```bash
        Do you want to use AWS CIDR Block:
        > yes
          no
    ```
    - give `ssh key pair name` name used for creating ssh key pair , `AMI Id` which depends on the AWS Region and the Operating System image you want to use.,
    - if you want to terminate all the resources on deletion then select  `on`
    ```bash
        Delete on termination should be:
        > off
          on
    ```
    - if you want to enable access log on AWS loadbalancers then select `yes`
    ```bash
        Do you want to Enable Access Logs on AWS Load Balancer:
        > yes
          no
    ```
    - give Automate FQDN example `chefautomate.example.com`
    - give Automte loadbalancer ARN, with the arn value of the Certificate created in AWS ACM for DNS entry of `chefautomate.example.com`.
    - give path of Automate loadbalance FQDN ssl root ca cerificates
    - set automate dashboard login password
    - set how many automate node want to have in cluster, recomended is atleast `two`
    - set automate instance type, recomended is `m5.large`
    - set automate EBS volume size, based on your load needs.
    - set automate EBS volume type, default is `gp3`, change as per your requirement.
    - set automate EBS volume IOPS, based on your load needs.
    - 
    - give Chef Server FQDN example `chefserver.example.com`
    - give Chef Server loadbalancer ARN, with the arn value of the Certificate created in AWS ACM for DNS entry of `chefinfraserver.example.com`.
    - give path of Chef Server loadbalance FQDN ssl root ca cerificates
    - set Chef Server dashboard login password
    - set how many Chef Server node want to have in cluster, recomended is atleast `two`
    - set Chef Server instance type, recomended is `m5.large`
    - set Chef Server EBS volume size, based on your load needs.
    - set Chef Server EBS volume type, default is `gp3`, change as per your requirement.
    - set Chef Server EBS volume IOPS, based on your load needs.
    - select `no` for chef managed database deloyment
    ```bash
        Do you want to use AWS Managed Databases:
         yes
        > no
    ```
    - set number of postgresql node you want, recomended is three nodes
    - set postgresql instance type, recomended is `m5.large`
    - set postgresql EBS volume size, based on your load needs.
    - set postgresql EBS volume type, default is `gp3`, change as per your requirement.
    - set postgresql EBS volume IOPS, based on your load needs.
  
    - set number of opensearch node you want, recomended is three nodes
    - set opensearch instance type, recomended is `m5.large`
    - set opensearch EBS volume size, based on your load needs.
    - set opensearch EBS volume type, default is `gp3`, change as per your requirement.
    - set opensearch EBS volume IOPs, based on your load needs.
    - If you want to configure database during deployment then select `yes` and provide detials accordingly for selected backup type, for S3 backup provide detials like `bucket name`.
    ```bash
    Backup need to be configured during deployment:
    > yes
      no
    Which backup option will you use:
    > AWS S3
      EFS
    ```
    all done we can find generated config file with name given in `config gen` command

    {{< note >}} Click [here](/automate/ha_cert_deployment) to know more on adding certificates for services during deployment. {{< /note >}}

2. Continue with the provisioning the infra after updating config:

    ```bash
    #Run commands as sudo.
    sudo -- sh -c "
    #Print data in the config
    cat config.toml

    #Run provision command to deploy `automate.aib` with set `config.toml`
    chef-automate provision-infra config.toml --airgap-bundle automate.aib
    "
    ```

3. Once the provisioning is successful, **if you have added custom DNS to your configuration file (`fqdn`), make sure to map the load-balancer FQDN from the output of previous command to your DNS from DNS Provider**. After that continue with the deployment process with following.

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

4. After the deployment successfully completed. To view the automate UI, run the command `chef-automate info`, you will get the `automate_url`.
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
After successful deployment we can proceed with node bootstraping please Refer [this](/automate/ha_node_bootstraping) docs.

### Sample config

{{< note >}}

-   Assuming 8+1 nodes (1 bastion, 2 for automate UI, 2 for Chef-server, 3 for Postgresql, 3 for Opensearch)

{{< /note >}}

{{< note >}}

-   User only needs to create/setup **the bastion node** with IAM role of Admin access, and s3 bucket access attached to it.

-   Following config will create s3 bucket for backup.

-   To provide multiline certificates use triple quotes like `"""multiline certificate contents"""`

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
    profile = "default"
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
    For example, if you have patched any external configurations like SAML or LDAP, or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, PostgreSQL, and OpenSearch.
-   The new node will be configured with the certificates which were already configured in your HA setup.

{{< /note >}}

{{< warning >}}
Downgrading the number of instance_count for the backend nodes will result in data loss. We do not recommend downgrading the backend nodes.
{{< /warning >}}

## Remove Single Node From Cluster on AWS Deployment

{{< warning >}}

-   We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](/automate/ha_onprim_deployment_procedure/#replace-node-in-automate-ha-cluster) for the reference.

-   Removal of nodes for PostgreSQL or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete PostgreSQL or OpenSearch nodes.

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
