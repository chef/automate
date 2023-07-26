+++
title = "On-Premise Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-Premise Deployment"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_onprim_deployment_procedure.md On-Premise Deployment"
    weight = 200
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This section will discuss the steps to deploy Chef Automate HA on-premise machines or on existing VMs. The steps are as follows:

## Install Chef Automate HA

### Prerequisites

- All VMs or Machines are up and running.
- OS Root Volume (/) must be at least 40 GB
- TMP space (/var/tmp) must be at least 5GB
- Separate Hab volume (/hab) provisioned at least 100 GB for OpenSearch node `/hab` volume will be more based on the data retention policy.
- A Common user has access to all machines.
- This common user should have sudo privileges.
- This common user uses the same SSH Private Key file to access all machines.
- Key-based SSH for the provisioning user for all the machines for HA-Deployment.
- We do not support passphrases for Private Key authentication.
- LoadBalancers are set up according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_on_premises_deployment_prerequisites/#firewall-checks)
- Make sure your linux has `sysctl` utility available in all Machines.
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, add the records to `/etc/hosts` in all the machines, including Bastion:

```bash
sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
```

- If the instance is **RedHat**, set SElinux config `enforcing` to `permissive` in all the nodes.\
  SSH to each node, then run:

```bash
sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config
```

{{< warning >}}

- PLEASE DONOT MODIFY THE WORKSPACE PATH it should always be "/hab/a2_deploy_workspace"
- We currently don't support AD managed users in nodes. We only support local linux users.
- If you have configured sudo password for the user, then you need to create an environment variable `sudo_password` and set the password as the value of the variable. Example: `export sudo_password=<password>`. And then run all sudo commands with `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.

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

    {{< note spaces=4 >}}
    Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
    {{< /note >}}

    Note: If the Airgapped Bastion machine is different, transfer the Bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the Airgapped Bastion Machine using the `scp` command.
    After transferring, in Airgapped Bastion, run the below commands:

    ```bash
    #Run commands as sudo.
    sudo -- sh -c "
    #Move the Chef Automate CLI to `/usr/bin`.
    cp -f chef-automate /usr/bin/chef-automate
    "
    ```

2. Generate config using below command and provide prompted details for config.

   ```bash
   chef-automate config gen config.toml
   ```
   - Select `Chef Automate HA` as topology
   - Select `On-Premise` as deployment type
   - Choose `Deployment` as config type
   - 
   - Provide ssh user name for ssh login
   - provide ssh group name of ssh user, default is same as name given for ssh user
   - Provide ssh port number for ssh login, default port is 22
   - Provide ssh login key file path, default path will be `/.ssh/id_rsa`
   - In case you have custom certificates for any service like Automate, Chef Infra Server, PostgreSQL, OpenSearch then choose `yes` otherwise select `no`, if you have selected `yes` then you will be prompt for root-certs, public certificates and private certificates for services later in flow
   - provide Automte FQDN example `automate.example.com`
   - provide ssl root certificate path for Automate FQDN
   - provide admin login password which you want to set for Automate dashboard,
   - Provide total number of node you want to keep for Automate node.
   - In case you havve custom certificates for Automate Node select `yes` other wise select `no`, 
   - If you have select `yes` for above then you will be prompt for do you have different certificates for each node, choose accordingly `yes` or `no`
     - Provide private key file path for Automate node
     - Provide public key file path for Automate node
   - Now it will ask for node IP Address for each Automate node


   - provide Chef Server FQDN example `chefinfraserver.example.com`
   - provide ssl root certificate path for Chef Server FQDN
   - Provide total number of node you want to keep for Chef Server node.
   - In case you havve custom certificates for Automate Node select `yes` other wise select `no`, 
   - If you have select `yes` for above then you will be prompt for do you have different certificates for each node, choose accordingly `yes` or `no`
     - Provide private key file path for Chef Server node
     - Provide public key file path for Chef Server node
   - Now it will ask for node IP Address for each Chef Server node
  
   - If you want to use External Databases like AWS managed databases or any other customer managed external databases then select `yes` other wise `no`
   - If you are using Chef managed databases then provide number of node you want to have for opensearch
   - Now In case you have custom certificates for opensearch then select `yes`
   - If you have different certificates for each opensearch node then select `no`
   - provide root-ca certificates for opensearch
   - provide admin certificates for opensearch
   - provide admin key certificates for opensearch
   - now provide private certificates, public certificates, and node ip for each node of opensearch on prompt

   - If you are using Chef managed databases then provide number of node you want to have for Postgresql
   - Now In case you have custom certificates for Postgresql then select `yes`
   - If you have different certificates for each Postgresql node then select `no`
   - provide root-ca certificates for Postgresql
   - provide admin certificates for Postgresql
   - provide admin key certificates for Postgresql
   - now provide private certificates, public certificates, and node ip for each node of Postgresql on prompt
  

   - If we want to use the same machine for OpenSearch and Postgresql, then provide the same IP for both config fields. This means overall; there will be three machines or VMs running both OpenSearch and Postgresql. A reduced performance should be expected with this. Use a minimum of 3 VMs or Machines for Both OpenSearch and Postgresql on all three machines.
   - Also, you can use the same machines for Chef Automate and Chef Infra Server. This means overall, there will be two machines or VMs running both Chef Automate and Chef Infra Server. A reduced performance should be expected with this. Minimum 2 VMs or Machines will be used by both Chef Automate and Chef Infra Server on both machines.
   - Thus, the overall minimum number of machines needed will be 5
   
 
   - select `yes` if you want to configure backup in config
   - select backup type from Aws S3, Minio, Object Storage, File System and NFS.
   - provide details for backup configurations like bucket name, access key, secrect key, url and region for s3, Minio or object storage, User must create the bucket themselves and make sure to assign correct [IAM policy for bucket access](/automate/backup/#aws-s3-permissions) if you are using AWS s3.
   - in case of NFS of File System backup provide backup location path.
   - Now all set, we can find generated config in config.toml file or file name provided for config gen command.
  
   {{< note >}} Click [here](/automate/ha_cert_deployment) to learn more about adding certificates for services during deployment. {{< /note >}}

2. Continue with the deployment after updating the config:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Print data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status summary
   "
   ```

   Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### Sample config

{{< note >}}

- Assuming 10+1 nodes (1 bastion, 2 for automate UI, 2 for Chef-server, 3 for Postgresql, 3 for Opensearch)
- Following config will by default leave the backup configuration empty
- To provide multiline certificates use triple quotes like `"""multiline certificate contents"""`

{{< /note >}}

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/jay-sydney-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "file_system"

[automate]
  [automate.config]
    admin_password = "Progress@123"
    fqdn = "automate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"

[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"

[opensearch]
  [opensearch.config]
    instance_count = "3"

[postgresql]
  [postgresql.config]
    instance_count = "3"

[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.3", "192.0.0.4"]
    opensearch_private_ips = ["192.0.0.5", "192.0.0.6", "192.0.0.7"]
    postgresql_private_ips = ["192.0.0.8", "192.0.0.9", "192.0.0.10"]
```

## On-Premise Setup with AWS Managed Services

### Prerequisites

- Follow the Prerequisites for On-Premise deployment. Click [here](#prerequisites) to know more.
- This deployment excludes the installation for Postgresql and OpenSearch as we are using the AWS Managed Services.
- Set up AWS RDS PostgreSQL 13.5-R1. Click [here](/automate/create_amazon_rds/) to know more. Open the required port in Security Groups while creating AWS RDS Postgresql.
- Set up AWS OpenSearch 1.3. Click [here](/automate/create_amazon_opensearch/) to know more.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#prerequisites) to know more.
- Create the Virtual Private Cloud (VPC) in AWS before starting or using default. Click [here](/automate/ha_vpc_setup/) to learn more about VPC and CIDR creation.
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) with privileges like: `AmazonS3FullAccess` and `AdministratorAccess`. Click [here](/automate/ha_iam_user/) to learn more about creating IAM Users.

See the steps [here](#run-these-steps-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

Update Config with relevant data. Click [here](#sample-config-to-setup-on-premise-deployment-with-aws-managed-services) for sample config of AWS Managed Services.

- Set AWS Config Details:
  - follow above steps to generate rest of configs, for AWS managed services `config gen` command will ask like 
    ```bash
     Are you going to use External Databases, like AWS RDS and AWS OpenSearch:
      > yes
        no
    ```
    select yes and choose which `AWS Managed` as type
    ```bash
    Type of External DB you will use:
    > AWS Managed
      Self Managed
    ```
  -  next `config gen` will ask for aws database details like `opensearch domain name`, `opensearch domain url`, `opensearch user name`, `opensearch user passwords`, provide above details as per aws managed database you have configured.

  -  Now it will ask do you want use default certificates of AWS, in case you have different certificates then default then select `no` and provide you own certificates, otherwise select `yes`
    ```bash 
     Do you want to use Default AWS Cert to connect with AWS Managed OpenSearch Domain URL:
     > yes
       no
    ```
   - now provide `aws opensearch snapshot arn`, `aws opensearch snapshot user accesskey`, `aws opensearch snapshot secrect key`, this values are required for to take backup from aws opensearch, please refer (/automate/managed_services/#enabling-opensearch-backup-restore) to create them and get their values.
   - We need to provide details of AWS managed postgresql (RDS), now it will ask for `RDS url and port` format will be `<url>:<port>`, `RDS postgresql super username`, `RDS postgresql super user password`, `RDS postgresql database username`, `RDS postgresql database user password`
   - Aws database have default ssl certificates, select `yes` if you want to use default certificates, If you have other then default certificates then select `no` and provide your own certificates.
    ```bash
      Do you want to use Default AWS Cert to connect with AWS Managed RDS PostgreSQL URL:
      > yes
        no
    ```
  - If you want to  conigure backup then select `yes` for backup configuration promot
    ```bash
      Backup need to be configured during deployment:
      > yes
        no
    ```
   for AWS managed database deployment backup options are only S3 as of now, please provide the detials of S3 like `bucket name`, `access key`, `secrect key`, `region`


Continue with the deployment after updating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status summary
   "
```

### Sample config to setup On-Premise Deployment with AWS Managed Services

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/jay-sydney-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "object_storage"

[object_storage]
  [object_storage.config]
    bucket_name = "fdjlfdsklfds"
    access_key = "CCAI..............."
    secret_key = "JVS................"
    endpoint = "https://s3.amazonaws.com"
    region = "us-east-2"

[automate]
  [automate.config]
    admin_password = "adminpassword"
    fqdn = "automate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"

[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"

[opensearch]
  [opensearch.config]
    instance_count = "0"

[postgresql]
  [postgresql.config]
    instance_count = "0"

[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.3", "192.0.0.4"]

[external]
  [external.database]
    type = "aws"
    [external.database.postgre_sql]
      instance_url = "pg.aws.com:5432"
      superuser_username = "masteruser"
      superuser_password = "masterpassword"
      dbuser_username = "dbusername"
      dbuser_password = "dbpassword"
    [external.database.open_search]
      opensearch_domain_name = "opensearchdomain"
      opensearch_domain_url = "os.aws.com"
      opensearch_username = "osuser"
      opensearch_user_password = "opensearchpassowrd"
      [external.database.open_search.aws]
        aws_os_snapshot_role_arn = "......"
        os_snapshot_user_access_key_id = "......"
        os_snapshot_user_access_key_secret = "......"
```

## On-Premise Setup with Self-Managed Services

### Prerequisites

- Follow the Prerequisites for On-Premise deployment. Click [here](#prerequisites).
- This deployment excludes the installation for Postgresql and OpenSearch as we are using the Self Managed services.

See the steps [here](#run-these-steps-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

Update Config with relevant data. Click [here](#sample-config-to-setup-on-premise-deployment-with-self-managed-services) for sample config for Self Managed Services.

- Set Self-Managed Config Details:
  - follow above steps to generate rest of configs, for AWS managed services `config gen` command will ask like 
    ```bash
     Are you going to use External Databases, like AWS RDS and AWS OpenSearch:
      > yes
        no
    ```
    select yes and choose which `AWS Managed` as type
    ```bash
    Type of External DB you will use:
      AWS Managed
    > Self Managed
    ```
   -  next `config gen` will ask for customer database details like `opensearch domain name`, `opensearch domain url:port`, `opensearch user name`, `opensearch user passwords`, provide above details as per aws managed database you have configured.

   - provide ssl root certificates path for opensearch
   - We need to provide details of customer postgresql, now it will ask for datbase `url and port` format will be `<url>:<port>`, `postgresql super username`, `postgresql super user password`, `postgresql database username`, `postgresql database user password`
   - provide ssl root certificates path for opensearch
   - If you want to  conigure backup then select `yes` for backup configuration promot
    ```bash
      Backup need to be configured during deployment:
      > yes
        no
    ```
   for customer managed database deployment backup options are S3, object storage and minio as of now, please provide the detials of like `bucket name`, `access key`, `secrect key`, `region`, `endpoint` etc.


Continue with the deployment after updating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status summary
   "
```

### Sample config to setup On-Premise Deployment with Self Managed Services

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/jay-sydney-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "object_storage"

[object_storage]
  [object_storage.config]
    bucket_name = "example-bucket"
    access_key = "JVS......."
    secret_key = "VIK........"
    endpoint = "https://objectstorage.example.com"

[automate]
  [automate.config]
    admin_password = "adminpassword"
    fqdn = "automate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"

[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"

[opensearch]
  [opensearch.config]
    instance_count = "0"

[postgresql]
  [postgresql.config]
    instance_count = "0"

[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.3", "192.0.0.4"]

[external]
  [external.database]
    type = "self-managed"
    [external.database.postgre_sql]
      instance_url = "pg.example.com:5432"
      superuser_username = "superusername"
      superuser_password = "superuserpassowrd"
      dbuser_username = "databaseusername"
      dbuser_password = "databaseuserpassword"
      postgresql_root_cert = "-----BEGIN CERTIFICATE-----
      -----END CERTIFICATE-----"
    [external.database.open_search]
      opensearch_domain_name = "opensearch-domain"
      opensearch_domain_url = "opensearch.example.com:9200"
      opensearch_username = "opensearchusername"
      opensearch_user_password = "opensearchuserpassword;"
      opensearch_root_cert = "-----BEGIN CERTIFICATE-----
      -----END CERTIFICATE-----"
```

## Add More Nodes to the OnPrem Deployment

The commands require some arguments so that it can determine which types of nodes you want to add to your HA setup from your bastion host. It needs the IP addresses of the nodes you want to add as comma-separate values with no spaces in between.
For example,

- if you want to add nodes with IP 10.1.2.23 to automate, you have to run the:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23
    ```

- If you want to add nodes with IP 10.1.2.23 and 10.0.1.42 to the chef-server, you have to run the:

    ```sh
    chef-automate node add --chef-server-ips 10.1.2.23,10.0.1.42
    ```

- If you want to add nodes with IP 10.1.2.23 and 10.0.1.42 to OpenSearch, you have to run the:

    ```sh
    chef-automate node add --opensearch-ips 10.1.2.23,10.0.1.42
    ```

- If you want to add nodes with IP 10.1.2.23, 10.0.1.54 and 10.0.1.42 to PostgreSQL you have to run:

    ```sh
    chef-automate node add --postgresql-ips 10.1.2.23,10.0.1.42,10.0.1.54
    ```

You can mix and match different services if you want to add nodes across various services.

- If you want to add nodes with IP 10.1.2.23 to automate and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL, you have to run:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23 --postgresql-ips 10.0.1.42,10.0.1.54
    ```

- If you want to add nodes with IP 10.1.2.23 to automate, nodes with IP 10.1.0.36 and 10.0.1.233 to chef-server, and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL you have to run:

    ```sh
    chef-automate node add --automate-ips 10.1.2.23 --chef-server-ips 10.1.0.36,10.0.1.233  --postgresql-ips 10.0.1.42,10.0.1.54
    ```

Once the command executes, it will add the supplied nodes to your automate setup. The changes might take a while.

- Make sure to update your loadbalancer configuration with the ip address of the new node. For reference check [Load Balancer Configuration page](/automate/loadbalancer_configuration/)

{{< note >}}

- If you have patched some external config to any of the existing services then make sure you apply the same on the new nodes as well.
For example, if you have patched any external configurations like SAML or LDAP, or any other done manually post-deployment in automate nodes, make sure to patch those configurations on the new automate nodes. The same must be followed for services like Chef-Server, Postgresql, and OpenSearch.
- The new node will be configured with the certificates which were already configured in your HA setup.
- If you had applied unique certificates per node, then the certificates of one of the nodes have been applied by default on the new nodes.
- If you want to change the certificates for the new nodes, you can manually run the `chef-automate cert-rotate [options]` command.

{{< /note >}}

{{< warning >}}
It's essential to ensure that the IP address of the nodes you are trying to add has sufficient resources and is reachable from the bastion host.
{{< /warning >}}

## Remove Single Node From Cluster on OnPrem Deployment

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](#replace-node-in-automate-ha-cluster) for the reference.

- Removal of nodes for Postgresql or OpenSearch is at your own risk and may result to data loss. Consult your database administrator before trying to delete Postgresql or OpenSearch nodes.

- Below process can be done for `chef-server` and `automate`.

- Only one node can be removed at a time irrespective of node type.

{{< /warning >}}

The command requires some arguments so that it can determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the IP address of the node you want to remove.

For example,

- If you want to remove node of automate, you have to run the:

    ```sh
    chef-automate node remove --automate-ip "<automate-ip-address>"
    ```

- If you want to remove node of chef-server, you have to run the:

    ```sh
    chef-automate node remove --chef-server-ip "<chef-server-ip-address>"
    ```

- If you want to remove node of OpenSearch, you have to run the:

    ```sh
    chef-automate node remove --opensearch-ip "<opensearch-ip-address>"
    ```

- If you want to remove node of PostgreSQL you have to run:

    ```sh
    chef-automate node remove --postgresql-ip "<postgresql-ip-address>"
    ```

Once the command executes, it will remove the supplied node from your HA setup. The changes might take a while.

- Make sure to remove the ip address of the deleted node from your loadbalancer configuration. For reference check [Load Balancer Configuration page](/automate/loadbalancer_configuration/)

{{< note >}}

- The IPs provided need to be associated with a node in your HA setup.
- Automate instance count cannot be less than 1.
- Chef Server instance count cannot be less than 1.
- Open search instance count cannot be less than 3.
- Postgresql instance count cannot be less than 3.
 {{< /note >}}

## Replace Node in Automate HA Cluster

- First Add a New Node follow [this](#add-more-nodes-to-the-onprem-deployment).
- Stop the Habitat Supervisior on the node .i.e going to be removed, use `systemctl stop hab-sup` command to stop the
  habitat supervisior.
- Remove a Existing Node follow [this](#remove-single-node-from-cluster-on-onprem-deployment).

## Uninstall chef automate HA

{{< danger >}}
The below section will uninstall the chef automate HA
{{< /danger >}}

### To uninstall On-Premise

To uninstall chef automate HA instances after unsuccessful deployment, run the below command in your bastion host.

```bash
    chef-automate cleanup --onprem-deployment
```

## Troubleshooting

### Failure to replacing nodes

  ```bash
  Error: Upload failed: scp: /var/automate-ha: Permission denied
  ```

- **Resolution**: Execute the below command.

  ```sh
  cd /hab/a2_deploy_workspace/terraform
  for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep module); do terraform taint $x; done
  cd -
   ```

- Once the module's tainted, run the `deploy` command again.

  ```sh
     chef-automate deploy config.toml --airgap-bundle <Path-to-the-airgap-bundle>
  ```
