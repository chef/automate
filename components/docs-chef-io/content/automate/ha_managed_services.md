## Prerequisites

Please refer the document section for [`Prerequisites`] ({{< ref "<ADD_PLACEHOLDER_FOR_AUTOMATE_DOCUMENT>" >}}) 

## Automate HA with managed services backup/restore

The following section provide steps to setup Automate HA cluster with external managed AWS PostgreSQL and OpenSearch service.
It also underlines steps required to take backup and restore with managed services External AWS PostgreSQL and OpenSearch service.

To deploy Chef Automate HA, execute the following steps in the listed order:

- Set up the Prerequisites for Chef Automate HA Deployment.
- Obtain an AWS account or if you already have one, sign on to your AWS account.
- Create an AWS Identity and Access Management IAM user.
- Create the certificate for the Chef Automate and Chef Server load balancers.
- Setup the Bastion Host AWS requirements.
- Configure Bastion Host for AWS.
- Connect your Bastion host to your AWS instance.
- Ensure you have Chef Automate utility installed, else download and install the latest version.
- Log in as a Root user by typing sudo su -.
- Execute the ./chef-automate init-config-ha aws command to set up the configuration for deployment. The config.toml configuration file generates and installs the latest deployment package with default settings.

- Update `config.toml` with setup_managed_services = true and update details of managed_opensearch and managed_rds parameters. Only automate and chef_server instances will be created by Automate HA provisioning:

  ```shell

  # This is a Chef Automate AWS HA mode configuration file. You can run
  # 'chef-automate deploy' with this config file and it should
  # successfully create a new Chef Automate HA instances with default settings.

  [architecture.aws]
  secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
  secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
  architecture = "aws"
  workspace_path = "/hab/a2_deploy_workspace"
  # ssh user name for ssh login to instance like default user for centos will centos or for red-hat will be ec2-user
  ssh_user = "ubuntu"
  # private ssh key file path to access instances
  ssh_key_file = "~/.ssh/<ssh-key-name>.pem"
  # sudo_password = ""
  # Backup config type can be efs and s3
  backup_config = "s3"
  # If s3 is selected for backup_config ,then uncomment and give s3_bucketName or else default chef-automate-ha.<deployment-string> will go
  s3_bucketName = "chef-automate-ha"

  # DON'T MODIFY THE BELOW LINE (backup_mount)
  backup_mount = "/mnt/automate_backups"


  [automate.config]
  # admin_password = ""
  # automate load balancer fqdn IP or path
  # fqdn = ""
  instance_count = "1"
  # teams_port = ""
  config_file = "configs/automate.toml"

  [chef_server.config]
  instance_count = "1"

  [opensearch.config]
  instance_count = "3"

  [postgresql.config]
  instance_count = "3"

  [aws.config]
  profile = "default"
  region = "<region>"
  # Provide vpcid and cidr block
  # E.g. aws_vpc_id = "vpc12318h"
  # E.g. aws_cidr_block_addr = "172.31.64.0"
  aws_vpc_id  = "<vpc-id>"
  aws_cidr_block_addr  = "<cidr-block-addr>"
  private_custom_subnets = []
  public_custom_subnets = []
  # ssh key pair name in AWS to access instances
  ssh_key_pair_name = "<ssh-key-name>"
  # If using external AWS managed service then set it true
  setup_managed_services = true
  # Fill managed opensearch details (required)if setup_managed_services = true
  managed_opensearch_domain_name = "<opensearch-domain-name>"
  managed_opensearch_domain_url = "<opensearch-domain-url>"
  managed_opensearch_username = "<opensearch-master-username>"
  managed_opensearch_user_password = "<opensearch-master-user-password>"
  managed_opensearch_certificate = ""
  # Following three parameters are optional and used by managed opensearch backup process
  # IAM snapshot Role ARN (optional). It will be created during provision if not provided.
  aws_os_snapshot_role_arn = ""
  # IAM user access key (optional). It will be created during provision if not provided.
  os_snapshot_user_access_key_id = ""
  # IAM user secret key (optional). It will be created during provision if not provided.
  os_snapshot_user_access_key_secret = ""
  # Fill managed postgresql details (required) if setup_managed_services = true
  managed_rds_instance_url = "<postgresql-endpoint>:5432"
  managed_rds_superuser_username = "<rds-superuser>"
  managed_rds_superuser_password = "<rds-superuser>"
  managed_rds_dbuser_username = "<rds-dbuser>"
  managed_rds_dbuser_password = "<rds-dbuser>"
  managed_rds_certificate = ""
  ami_filter_name = ""
  ami_filter_virt_type = ""
  ami_filter_owner = ""
  ami_id = "<ami-id>"
  # Enable/Disable load balancer logs
  lb_access_logs = "false"
  automate_server_instance_type = "t3.medium"
  chef_server_instance_type = "t3.medium"
  opensearch_server_instance_type = "m5.large"
  postgresql_server_instance_type = "t3.medium"
  automate_lb_certificate_arn = ""
  chef_server_lb_certificate_arn = ""
  automate_ebs_volume_iops = "100"
  automate_ebs_volume_size = "50"
  automate_ebs_volume_type = "gp3"
  chef_ebs_volume_iops = "100"
  chef_ebs_volume_size = "50"
  chef_ebs_volume_type = "gp3"
  opensearch_ebs_volume_iops = "100"
  opensearch_ebs_volume_size = "50"
  opensearch_ebs_volume_type = "gp3"
  postgresql_ebs_volume_iops = "100"
  postgresql_ebs_volume_size = "50"
  postgresql_ebs_volume_type = "gp3"
  X-Contact = ""
  X-Dept = ""
  X-Project = ""

  ```

- Create the airgap bundle with latest version. It downloads and bundles the software `automate-<version>.aib` in the bastion host.

  ```shell
  ./chef-automate airgap bundle create
  ```

- Provision the infrastructure. Below command downloads Chef Habitat creates deployment workspace (/hab/a2_deploy_workspace), and provisions AWS infrastructure.

  ```shell
  ./chef-automate provision-infra config.toml --airgap-bundle automate-<version>.aib
  ```

- Deploy automate HA by running below command. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

  ```shell
  ./chef-automate deploy config.toml --airgap-bundle automate-<version>.aib
  ```

{{< note >}}
Ensure that you have completed prerequisites step `Map the snapshot role in OpenSearch Dashboards` of OpenSearch Setup before starting backup. Either you can map user/role created and passed in config.toml or map the generated user/role during provision if not provided in config.toml
{{< /note >}}

### Create a Backup

Make a backup with the [`backup create`]({{< ref "cli_chef_automate/#chef-automate-backup-create" >}}) command. You can create it by running the backup command from a **Chef Automate front-end node**. The backup command is as shown below:

```shell
chef-automate backup create
```

The output shows the backup progress for each service.
A successful backup displays a success message containing the timestamp of the backup:

```shell
Success: Created backup 20180518010336
```

### List Backups

You can list existing backups with the [`backup list`]({{< ref "cli_chef_automate/#chef-automate-backup-list" >}}) command:

```shell
chef-automate backup list
```

The output shows each backup and its age:

```shell
        Backup        State  Age
20180508201548    completed  8 minutes old
20180508201643    completed  8 minutes old
20180508201952    completed  4 minutes old
```

By default, this command communicates with your running Chef Automate installation to list the backups.
If the Chef Automate installation is down, you can still list the backups.

For backups stored in an AWS S3 bucket, use:

```shell
chef-automate backup list s3://bucket_name/base_path
```

### Restoring the S3 Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (s3), follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing below command:

  ```shell
  chef-automate status
  ```

- Shutdown Chef Automate service on all front-end nodes by executing the below command:

  ```shell
  sudo systemctl stop chef-automate
  ```

- Log in to the same instance of Chef Automate **front-end node from which backup is taken**.

- Restore the backup data using below command:

  ```shell
  chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"
  ```

- Start all Chef Automate and Chef Infra Server front-end nodes by executing the below command.

  ```shell
  sudo systemctl start chef-automate
  ```
