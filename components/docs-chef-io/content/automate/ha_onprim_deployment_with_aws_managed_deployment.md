+++
title = "On-premise Deployment with AWS Managed Database​"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-premise Deployment with AWS Managed Database​"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_onprim_deployment_with_aws_managed_deployment.md On-premise Deployment with AWS Managed Database​"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This section will discuss deploying Chef Automate HA on-premise machines with AWS Managed Database. The steps are as follows:

## Prerequisites

- Refer to the [On-premises Deployment with Chef Managed Database](/automate/ha_onprim_deployment_procedure/)​ page for the on-premises deployment specific prerequisites.
- This deployment excludes the installation for Postgresql and OpenSearch as we are using the AWS Managed Services.
- Set up AWS RDS PostgreSQL 13.5-R1. Click [here](/automate/create_amazon_rds/) to know more. Open the required port in Security Groups while creating AWS RDS Postgresql.
- Set up AWS OpenSearch 1.3. Click [here](/automate/create_amazon_opensearch/) to know more.
- For Backup and Restore with Managed Service. Click [here](/automate/managed_services/#prerequisites) to know more.
- Create the Virtual Private Cloud (VPC) in AWS before starting or using default. Click [here](/automate/ha_vpc_setup/) to learn more about VPC and CIDR creation.
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) with privileges like: `AmazonS3FullAccess` and `AdministratorAccess`. Click [here](/automate/ha_iam_user/) to learn more about creating IAM Users.

See the steps [here](/automate/ha_onprim_deployment_procedure/#steps-to-run-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

## Steps to Generate Config

1. Generate config using the below command:

    ```bash
    sudo chef-automate config gen config.toml
    ```

    Click [here](/automate/ha_config_gen) to know more about generating config

    {{< note >}} You can also generate config using **init config** and then generate init config for existing infrastructure. The command is as shown below:

    `chef-automate init-config-ha existing_infra`{{< /note >}}

## Config Verify

1. We verify the above config using the below command:

    ```bash
    sudo chef-automate verify -c config.toml
    ```

    To know more about config verify you can check [Config Verify Doc page](/automate/ha_verification_check/).

    Once the verification is successfully completed, then proceed with deployment, In case of failure please fix the issue and re-run the verify command.

## Steps to Deploy

Continue with the deployment after generating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml
   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib
   "
```

## Verify Deployment

Verify the deployment by checking status summary:

```bash
    sudo -- sh -c "
    #After Deployment is done successfully. Check the status of Chef Automate HA services
    chef-automate status summary
    "
```

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

After successful deployment, proceed with following:

   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping,  Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstraping.

## Sample Config to setup On-Premise Deployment with AWS Managed Services

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "~/.ssh/my-key.pem"
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
    fqdn = "chefautomate.example.com"
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
        aws_os_snapshot_role_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
        os_snapshot_user_access_key_id = "CCAI..............."
        os_snapshot_user_access_key_secret = "JVS................"
```

## Uninstall Chef Automate HA

To uninstall Chef Automate HA instances after unsuccessful deployment, run the below command in your bastion host.

```bash
    chef-automate cleanup --onprem-deployment
```
