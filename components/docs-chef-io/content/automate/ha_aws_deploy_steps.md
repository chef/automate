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

- Virtual Private Cloud (VPC) should be created in AWS before starting. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- If you want to use Default VPC, we have to create public and private subnets, If subnets are unavailable. Please refer [this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
- We need three private and three public subnets in a vpc (1 subnet for each AZ). As of now, we support a dedicated subnet for each AZ.
- We recommend creating a new vpc. And Bastion should be in the same VPC.
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

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`. Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS so new VMs are created using that pair. Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
- We do not support passphrases for Private Key authentication.
- Preferred key type will be ed25519
- Ensure your Linux has the `sysctl` utility available in all nodes.

{{< warning >}}

- PLEASE DO NOT MODIFY THE WORKSPACE PATH. It should always be "/hab/a2_deploy_workspace".
- We currently don't support AD managed users in nodes. We only support local Linux users.
- If you have configured a sudo password for the user, you must create an environment variable `sudo_password` and set the password as the variable's value. Example: `export sudo_password=<password>`. And then, run all sudo commands with the `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.

{{< /warning >}}

### Deployment

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

    {{< note >}} Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever. {{< /note >}}

##### Steps to generate config
1. Generate config with relevant data using the below command:

    ```bash
    sudo -- sh -c "
    chef-automate config gen config.toml
    "
    ```
    Click [here](/automate/ha_config_gen) to know more about generating config


##### Steps to provision

1. Continue with the provisioning of the infra after generating the config:

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
    #After Deployment is done successfully. Check the status of Chef Automate HA services
    chef-automate status summary
    #Check Chef Automate HA deployment information using the following command
    chef-automate info
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

3. After the deployment is completed. To view the automate UI, run the command `chef-automate info`, and you will get the `automate_url`. If you want to change the FQDN URL from the loadbalancer URL to some other FQDN URL, then use the below template.

- create a file `a2.fqdn.toml`

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

After successful deployment, proceed with following...
   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping,  Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstraping.

### Sample config

{{< note >}}
Assuming 10+1 nodes (1 bastion, 2 for automate UI, 2 for Chef-server, 3 for Postgresql, 3 for OpenSearch)
{{< /note >}}

{{< note >}}

- User only needs to create/set up **the bastion node** with the IAM role of Admin access and s3 bucket access attached.
- The following config will create an s3 bucket for backup.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`

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
    profile = "default"  #This should be commented incase if IAM role is attached
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

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. When you run the command, it needs the count of the nodes you want to add as an argument. For example,

- If you want to add two nodes to automate, you have to run the:

    ```sh
    chef-automate node add --automate-count 2
    ```

- If you want to add three nodes to the chef-server, you have to run the:

    ```sh
    chef-automate node add --chef-server-count 3
    ```

- If you want to add one node to OpenSearch, you have to run the:

    ```sh
    chef-automate node add --opensearch-count 1
    ```

- If you want to add two nodes to PostgreSQL, you have to run the:

    ```sh
    chef-automate node add --postgresql-count 2
    ```

You can mix and match different services to add nodes across various services.

- If you want to add one node to automate and two nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-count 1 --postgresql-count 2
    ```

- If you want to add one node to automate, two nodes to chef-server, and two nodes to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-count 1 --chef-server-count 2 --postgresql-count 2
    ```

Once the command executes, it will add the supplied nodes to your automated setup. The changes might take a while.

{{< note >}}

- If you have patched some external config to any existing services, then apply the same on the new nodes. For example, if you have patched any external configurations like SAML or LDAP or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, Postgresql, and OpenSearch.
- The new node will be configured with the certificates already configured in your HA setup.
{{< /note >}}

{{< warning >}}
Downgrading the number of instance_count for the backend nodes will result in data loss. We do not recommend downgrading the backend nodes.
{{< /warning >}}

## Remove Single Node From Cluster on AWS Deployment

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](/automate/ha_onprim_deployment_procedure/#replace-node-in-automate-ha-cluster) for the reference.
- Removal of nodes for Postgresql or OpenSearch is at your own risk and may result in data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.
- Below process can be done for `chef-server` and `automate`.

{{< /warning >}}

The command requires some arguments to determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the node's IP address you want to remove as an argument when you run the command. For example,

- If you want to remove the node of automate, you have to run the:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- If you want to remove the node of the chef-server, you have to run the:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- If you want to remove the node of OpenSearch, you have to run the:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- If you want to remove the node of PostgreSQL, you have to run the:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the supplied node from your HA setup. The changes might take a while.

## Uninstall Chef automate HA

{{< danger >}}

- Running the clean-up command will remove all AWS resources created by the `provision-infra` command
- Adding the `--force` flag will remove storage (Object Storage/ NFS) if it is created by provision-infra`.

{{< /danger >}}

To uninstall Chef Automate HA instances after unsuccessful deployment, run the below command in your bastion host.

```bash
chef-automate cleanup --aws-deployment --force
```

OR

```bash
chef-automate cleanup --aws-deployment
```
