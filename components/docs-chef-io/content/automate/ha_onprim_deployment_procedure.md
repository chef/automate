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
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
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

{{< warning >}} PLEASE DONOT MODIFY THE WORKSPACE PATH it should always be "/hab/a2_deploy_workspace"
{{< /warning >}}

### Run these steps on Bastion Host Machine

1. Run the below commands to download the latest Automate CLI and Airgapped Bundle:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Download Chef Automate CLI.
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip
   | gunzip - > chef-automate && chmod +x chef-automate
   | cp -f chef-automate /usr/bin/chef-automate

   #Download the latest Airgapped Bundle.
   #To download specific version bundle, example version: 4.2.59 then replace latest.aib with 4.2.59.aib
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate.aib

   #Generate init config and then generate init config for existing infrastructure
   chef-automate init-config-ha existing_infra
   "
   ```

  {{< note >}} Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever. {{< /note >}}

   Note: If the Airgapped Bastion machine is different, transfer the Bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the Airgapped Bastion Machine using the `scp` command.
   After transferring, in Airgapped Bastion, run the below commands:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Move the Chef Automate CLI to `/usr/bin`.
   cp -f chef-automate /usr/bin/chef-automate

   #Generate init config and then generate init config for existing infrastructure
   chef-automate init-config-ha existing_infra
   "
   ```

1. Update Config with relevant data. Click [here](/automate/ha_onprim_deployment_procedure/#sample-config) for sample config

   ```bash
   vi config.toml
   ```

   - Add No. of machines for each Service: Chef Automate, Chef Infra Server, Postgresql, OpenSearch
   - Add the IP address of each machine in the relevant service section; multiple IPs should be in double quotes (`"`) and separated with a comma (`,`). Example: `["10.0.0.101","10,0.0.102"]`
      - If we want to use the same machine for OpenSearch and Postgresql, then provide the same IP for both config fields. This means overall; there will be three machines or VMs running both OpenSearch and Postgresql. A reduced performance should be expected with this. Use a minimum of 3 VMs or Machines for Both OpenSearch and Postgresql on all three machines.
      - Also, you can use the same machines for Chef Automate and Chef Infra Server. This means overall, there will be two machines or VMs running both Chef Automate and Chef Infra Server. A reduced performance should be expected with this. Minimum 2 VMs or Machines will be used by both Chef Automate and Chef Infra Server on both machines.
      - Thus, the overall minimum number of machines needed will be 5.
   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give the `ssh_key_file` path; this key should have access to all the Machines or VMs.
   - `sudo_password` is only meant to switch to sudo user. If you have configured a password for the sudo user, please provide it here.
   - We support only private key authentication.
   - Provide `backup_config` based on the type of backup storage you have. This field can be optionally left empty during deployment and can be patched at later point. Allowed values are `object_storage` and `file_system`.
   - If `backup_config` is `object_storage`, make sure to fill values under `[object_storage.config]`
   - Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VM's. Example: `chefautomate.example.com`
   - Set the `admin_password` to what you want to use to login to Chef Automate, when you open up `chefautomate.example.com` in the Browser, for the username `admin`.

   {{< note >}} Click [here](/automate/ha_cert_deployment) to learn more about adding certificates for services during deployment. {{< /note >}}

1. Continue with the deployment after updating the config:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Print data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status
   ```

   Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### Sample config

{{< note >}}

- Assuming 10+1 nodes (1 bastion, 2 for automate UI, 2 for Chef-server, 3 for Postgresql, 3 for Opensearch)
- Following config will by default leave the backup configuration empty

{{< /note >}}

```config
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file, and it should
# successfully create a new Chef Automate HA instance with default settings.
[architecture.existing_infra]
ssh_user = ""
# private ssh key file path to access instances
# Eg.: ssh_user = "~/.ssh/A2HA.pem"
ssh_key_file = ""
ssh_port = "22"
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "existing_nodes"
# DON'T MODIFY THE BELOW LINE (workspace_path)
workspace_path = "/hab/a2_deploy_workspace"
backup_mount = "/mnt/automate_backups"

# Eg.: backup_config = "object_storage" or "file_system"
backup_config = ""
# If backup_config = "object_storage" fill out [object_storage.config] as well 
[object_storage.config]
bucket_name = ""
access_key = ""
secret_key = ""
endpoint = ""
# [Optional] Mention object_storage region if applicable
# Eg: region = "us-west-1"
region = ""
# ============== EC2 Nodes Config ======================
[automate.config]
# Automate Load Balancer FQDN eg.: "chefautomate.example.com"
fqdn = ""
instance_count = "2"
config_file = "configs/automate.toml"
# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Automate load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""
# Or you can provide certificates at the node level using the below fields
# [[automate.config.certs_by_ip]]
# ip = "A.B.C.D"
# private_key = ""
# public_key = ""
[chef_server.config]
instance_count = "2"
# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Chef Server load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""
# Or you can provide certificates at the node level using the below fields
# [[chef_server.config.certs_by_ip]]
# ip = "I.J.K.L"
# private_key = ""
# public_key = ""
[opensearch.config]
instance_count = "3"
# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add OpenSearch load balancer root-ca and keys
# root_ca = ""
# admin_key = ""
# admin_cert = ""
# private_key = ""
# public_key = ""
# Or you can provide certificates at the node level using the below fields
# [[opensearch.config.certs_by_ip]]
# ip = "A1.A2.A3.A4"
# private_key = ""
# public_key = ""
[postgresql.config]
instance_count = "3"
# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Postgresql load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""
# Or you can provide certificates at the node level using the below fields
# [[postgresql.config.certs_by_ip]]
# ip = "D1.D2.D3.D4"
# private_key = ""
# public_key = ""
[existing_infra.config]
## === INPUT NEEDED ===
# provide comma separate IP address of nodes, like ["192.0.0.1", "192.0.0.2", "192.0.0.2"]
# No of IP address should be same as No of instance_count count mentioned above in
# automate.config, chef_server.config, opensearch.config, and postgresql.config
automate_private_ips = ["A.B.C.D","D.E.F.G"]
chef_server_private_ips = ["I.J.K.L","M.N.O.P"]
opensearch_private_ips = ["A1.A2.A3.A4","B1.B2.B3.B4","C1.C2.C3.C4"]
postgresql_private_ips = ["D1.D2.D3.D4","E1.E2.E3.E4","F1.F2.F3.F4"]
```

### Minimum changes to be made for On-Premise Deployment

- Give `ssh_user` which has access to all the machines. Eg: `ubuntu`, `centos`, `ec2-user`
- Give the `ssh_key_file` path; this key should have access to all the Machines or VMs. Eg: `~/.ssh/id_rsa`, `/home/ubuntu/key.pem`
- Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VMs. E.g.: `chefautomate.example.com`
- `automate_private_ips` Eg: ["192.0.0.1"]
- `chef_server_private_ips` Eg: ["192.0.1.1"]
- `opensearch_private_ips` Eg: ["192.0.2.1", "192.0.2.2", "192.0.2.2"]
- `postgresql_private_ips` Eg: ["192.0.3.1", "192.0.3.2", "192.0.3.2"]
- *Optional - In case of adding a backup configuration, make sure to fill in the following format in the sample. 
    - For **Object Storage** - `backup_config = "object_storage"`. Other variables to be filled - in are `bucket_name`, `access_key`,`secret_key`, `endpoint`, and `region`. 
    - For **File System** - `backup_config = "file_system"`.

## On-Premise Setup with AWS Managed Services

### Prerequisites

- Follow the Prerequisites for On-Premise deployment. Click [here](https://docs.chef.io/automate/ha_onprim_deployment_procedure/#prerequisites) to know more.
- This deployment excludes the installation for Postgresql and OpenSearch as we are using the AWS Managed Services.
- Set up AWS RDS Postgresql 13.5. Click [here](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html) to know more. Open the required port in Security Groups while creating AWS RDS Postgresql.
- Set up AWS OpenSearch 1.2. Click [here](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html) to know more.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#prerequisites) to know more.
- Create the Virtual Private Cloud (VPC) in AWS before starting or using default. Click [here](/automate/ha_vpc_setup/) to learn more about VPC and CIDR creation.
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) with privileges like: `AmazonS3FullAccess` and `AdministratorAccess`. Click [here](/automate/ha_iam_user/) to learn more about creating IAM Users.

See the steps [here](https://docs.chef.io/automate/ha_onprim_deployment_procedure/#run-these-steps-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

Update Config with relevant data. Click [here](/automate/ha_onprim_deployment_procedure/#sample-config-to-setup-on-premise-deployment-with-aws-managed-services) for sample config of AWS Managed Services.

- Set AWS Config Details:

  - Provide instance count as `0` for both [opensearch.config] and [postgresql.config] and leave the values of opensearch_private_ips and postgresql_private_ips as an empty array.
  - Set `type` as `aws`, as these deployment steps are for Managed Services AWS Deployment. The default value is blank, which should change.
  - Set `instance_url`, `superuser_username`, `superuser_password`, `dbuser_username`, `dbuser_password` for the **Managed AWS RDS Postgresql** created in the Prerequisite steps.
  - Set `instance_url` as the URL with Port No. For example: `"database-1.c2kvay.eu-north-1.rds.amazonaws.com:5432"`
  - Set `opensearch_domain_name`, `opensearch_domain_url`, `opensearch_username`, `opensearch_user_password` for the **Managed AWS OpenSearch** created in the Prerequisite steps.
  - Set `opensearch_domain_url` as the URL without Port No. For example: `"vpc-automate-ha-cbyqy5q.eu-north-1.es.amazonaws.com"`.
  - Leave postgresql_root_cert and opensearch_root_cert blank in case of On-Premise with AWS Managed Services.
  - For backup and restore configuration set `aws_os_snapshot_role_arn`, `os_snapshot_user_access_key_id`, `os_snapshot_user_access_key_secret`. Click [here](/automate/managed_services/#prerequisites) to know more.

Continue with the deployment after updating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status
```

### Sample config to setup On-Premise Deployment with AWS Managed Services

```config
# ============== External Database Services ======================
## === INPUT NEEDED ===
# In case you are trying to deploy with AWS Managed Services, set the type as "aws"
# If you are trying an externally managed database set type as "self-managed"
[external.database]
# eg type = "aws" or "self-managed"
type = "aws"
[external.database.postgre_sql]
# eg: instance_url = "managed-rds-db.cww4poze5gkx.ap-northeast-1.rds.amazonaws.com:5432"
instance_url = ""
# eg: username = "postgres"
superuser_username = ""
# eg: password = "Progress123"
superuser_password = ""
# eg: dbuser_username = "postgres"
dbuser_username = ""
# eg: dbuser_password = "Progress123"
dbuser_password = ""
# In the case of AWS-managed RDS, leave it blank
# eg: postgresql_root_cert = "<cert_content>"
postgresql_root_cert = ""
[external.database.open_search]
# eg: managed_opensearch_domain_name = "managed-services-os"
opensearch_domain_name = ""
# eg: opensearch_domain_url = "search-managed-services-os-eckom3msrwqlmjlgbdu.us-east-1.es.amazonaws.com"
opensearch_domain_url = ""
# eg: opensearch_username = "admin"
opensearch_username = ""
# eg: opensearch_user_password = "Progress@123"
opensearch_user_password = ""
# In the case of AWS-managed OpenSearch, leave it blanks
# eg: opensearch_root_cert = "<cert_content>"
opensearch_root_cert = ""
[external.database.open_search.aws]
# eg: aws_os_snapshot_role_arn = "arn:aws:iam::1127583934333:role/managed-services"
aws_os_snapshot_role_arn = ""
# eg: os_snapshot_user_access_key_id = "AKIA..........PQS7Q7A"
os_snapshot_user_access_key_id = ""
# eg: os_snapshot_user_access_key_secret = "skP4Mqihj....................anAXAX"
os_snapshot_user_access_key_secret = ""
```

## On-Premise Setup with Self-Managed Services

### Prerequisites

- Follow the Prerequisites for On-Premise deployment. Click [here](https://docs.chef.io/automate/ha_onprim_deployment_procedure/#prerequisites).
- This deployment excludes the installation for Postgresql and OpenSearch as we are using the Self Managed services.

See the steps [here](https://docs.chef.io/automate/ha_onprim_deployment_procedure/#run-these-steps-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

Update Config with relevant data. Click [here](/automate/ha_onprim_deployment_procedure/#sample-config-to-setup-on-premise-deployment-with-self-managed-services) for sample config for Self Managed Services.

- Set Self-Managed Config Details:
  - Provide instance count as `0` for both [opensearch.config] and [postgresql.config] and leave the values of opensearch_private_ips and postgresql_private_ips as an empty array.
  - Set `type` as `self-managed`, as these deployment steps are for Managed Services AWS Deployment. The default value is blank, which should change.
  - Set `instance_url`, `superuser_username`, `superuser_password`, `dbuser_username`, `dbuser_password` for your Self Managed RDS.
  - Set `instance_url` as the URL with Port No. For example: `"10.1.2.189:7432"`.
  - Provide the Root ca value of Postgresql `postgresql_root_cert`.
  - Set `opensearch_domain_name`, `opensearch_domain_url`, `opensearch_username`, `opensearch_user_password` for your Self Managed OpenSearch.
  - Set `opensearch_domain_url` as the URL with Port No. For example: `"10.1.2.234:9200"`.
  - Provide the Root ca value of OpenSearch `opensearch_root_cert`.
  - Leave the [external.database.open_search.aws] config as blank as it is specific for AWS Managed Services.

Continue with the deployment after updating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check the status of Chef Automate HA services
   chef-automate status
```

### Sample config to setup On-Premise Deployment with Self Managed Services

```config
# ============== External Database Services ======================
## === INPUT NEEDED ===
# In case you are trying to deploy with AWS Managed Services, set the type as "aws"
# If you are trying an externally managed database set type as "self-managed"
[external.database]
# eg type = "aws" or "self-managed"
type = "self-managed"
[external.database.postgre_sql]
# eg: instance_url = "A.B.C.D:7432"
instance_url = ""
# eg: username = "postgres"
superuser_username = ""
# eg: password = "Progress123"
superuser_password = ""
# eg: dbuser_username = "postgres"
dbuser_username = ""
# eg: dbuser_password = "Progress123"
dbuser_password = ""
# In the case of AWS-managed RDS, leave it blank
# eg: postgresql_root_cert = "<cert_content>"
postgresql_root_cert = ""
[external.database.open_search]
# eg: managed_opensearch_domain_name = "chefnode"
opensearch_domain_name = ""
# eg: opensearch_domain_url = "A.B.C.D:9200"
opensearch_domain_url = ""
# eg: opensearch_username = "admin"
opensearch_username = ""
# eg: opensearch_user_password = "Progress@123"
opensearch_user_password = ""
# In the case of AWS-managed OpenSearch, leave it blanks
# eg: opensearch_root_cert = "<cert_content>"
opensearch_root_cert = ""
[external.database.open_search.aws]
# eg: aws_os_snapshot_role_arn = "arn:aws:iam::1127583934333:role/managed-services"
aws_os_snapshot_role_arn = ""
# eg: os_snapshot_user_access_key_id = "AKIA..........PQS7Q7A"
os_snapshot_user_access_key_id = ""
# eg: os_snapshot_user_access_key_secret = "skP4Mqihj....................anAXAX"
os_snapshot_user_access_key_secret = ""
```

### How To Add More Nodes to the OnPrem Deployment

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

### How To Remove Any Nodes From Frontend Cluster OnPrem Deployment

{{< warning >}}

- We do not recommend the removal of any node from the backend cluster, but replacing the node is recommended. For the replacement of a node, click [here](/automate/ha_onprim_deployment_procedure/#How-to-Replace-Node-in-Automate-HA-Cluster) for the reference.

- Below process can be done for `chef-server` and `automate`.

{{< /warning >}}

The commands require some arguments so that it can determine which types of nodes you want to remove from your HA setup from your bastion host. It needs the IP addresses of the nodes you want to remove as comma-separate values with no spaces in between.

For example,

- if you want to remove nodes with IP 10.1.2.23 to automate, you have to run the:

    ```sh
    chef-automate node remove --automate 10.1.2.23
    ```

- If you want to remove nodes with IP 10.1.2.23 and 10.0.1.42 to chef-server you have to run the:

    ```sh
    chef-automate node remove --chef-server 10.1.2.23,10.0.1.42
    ```

- If you want to remove nodes with IP 10.1.2.23 and 10.0.1.42 to OpenSearch, you have to run:

    ```sh
    chef-automate node remove --opensearch 10.1.2.23,10.0.1.42
    ```

  - If you want to remove nodes with IP 10.1.2.23, 10.0.1.54 and 10.0.1.42 to PostgreSQL you have to run:

    ```sh
    chef-automate node remove --postgresql 10.1.2.23,10.0.1.42,10.0.1.54
    ```

You can mix and match different services to remove nodes across various services.

- If you want to remove nodes with IP 10.1.2.23 to automate and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL, you have to run:

    ```sh
    chef-automate node remove --automate 10.1.2.23 --postgresql 10.0.1.42,10.0.1.54
    ```

- If you want to remove nodes with IP 10.1.2.23 to automate, nodes with IP 10.1.0.36 and 10.0.1.233 to chef-server, and nodes with IP 10.0.1.54 and 10.0.1.42 to PostgreSQL you have to run:

    ```sh
    chef-automate node remove --automate 10.1.2.23 --chef-server 10.1.0.36,10.0.1.233  --postgresql 10.0.1.42,10.0.1.54
    ```

Once the command executes, it will remove the supplied nodes from your automate setup. The changes might take a while.

{{< note >}}

- The IPs provided need to be associated with a node in your HA setup.
- Automate instance count cannot be less than 1.
- Chef Server instance count cannot be less than 1.
- Open search instance count cannot be less than 3.
- Postgresql instance count cannot be less than 3.
 {{< /note >}}

### How to Replace Node in Automate HA Cluster

- First Add a New Node follow [this](#How-To-Add-More-Nodes-to-the-OnPrem-Deployment).
- Stop the Habitat Supervisior on the node .i.e going to be removed, use `systemctl stop hab-sup` command to stop the
  habitat supervisior.
- Remove a Existing Node follow [this](#How-To-Remove-Any-Nodes-From-Frontend-Cluster-OnPrem-Deployment).

### Uninstall chef automate HA

{{< danger >}}
The below section will uninstall the chef automate HA
{{< /danger >}}

#### To uninstall On-Premise

To uninstall chef automate HA instances after unsuccessful deployment, run the below command in your bastion host.

```bash
    chef-automate cleanup --onprem-deployment
```

### Troubleshooting

#### Failure to replacing nodes

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
