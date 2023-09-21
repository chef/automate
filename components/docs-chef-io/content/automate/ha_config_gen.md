+++
title = "Automate HA Config Generation"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Automate Config Generation"
    parent = "automate/deploy_high_availability/manage_ha_cluster"
    identifier = "automate/deploy_high_availability/manage_ha_cluster/ha_config_gen.md Automate Config Generation"
    weight = 220
+++
{{< warning >}}
 {{% automate/ha-warn %}}
{{< /warning >}}

## Command Usage

```bash
 chef-automate config gen config.toml
```

`config.toml` is the file where all the generated config will be saved at the end, you can choose to not provide this then the output will be shown to stdout.

Refer the fields below to generate Chef Automate High Availability (HA) configuration using `chef-automate config gen` command.

## Automate HA Topology

You need to have `Chef Automate HA` as a topology for HA deployments.

`On-Premise`
: To deploy on customer created Chef Automate HA cluster.

`Aws`
: To deploy in AWS environment. Cluster will be created by Automate HA.

`Deployment`
: The configuration type.

## SSH User and Group

`ssh user name`
: User name to SSH to cluster instances.

`ssh group name`
: Group name which is associated with SSH user.

`ssh port no`
: Port to connect using SSH. Default value: `22`.

`ssh key file path`
: SSH key file path, same will be used to SSH to cluster instances. For example, `/home/ec2-user/KEY_FILENAME.pem`.

## Automate Load Balancer FQDN

`Automate FQDN`
: Chef Automate FQDN. For example, `chefautomate.example.com`.

`Automate FQDN ARN`
: For AWS deployment ARN name is required for Automate FQDN domain.

`Automate FQDN Root Certificate`
: SSL root certificate for Automate FQDN domain.

## Automate Admin

`Automate Admin Password`
: Admin password to login to automate dashboard.

## Chef Infra Server Load Balancer FQDN

`Chef Server FQDN`
: Chef Automate FQDN. For example, `chefserver.example.com`.

`Chef Server FQDN ARN`
: For Aws deployment ARN name is required for Chef Servers FQDN domain.

`Chef Server FQDN Root Certificate`
: SSL root certificate for Chef Infra Server FQDN domain.

## Number of Nodes in Automate HA Cluster

`Automate node count`
: Number of nodes we want to keep for automate, in case of On-Premise deployment need to provide IP Address for all nodes.

`Chef Server node count`
: Number of nodes we want to keep for Chef Server, in case of On-Premise deployment need to provide IP Address for all nodes.

`Opensearch node count`
: Number of nodes we want to keep for Opensearch, in case of On-Premise deployment need to provide IP Address for all nodes.

`Postgresql node count`
: Number of nodes we want to keep for Postgresql, in case of On-Premise deployment need to provide IP Address for all nodes.

## Private/Public Key For Automate

`Private key for Automate`
: If you have a custom certificate for Automate node provide your private for Automate, If you have a custom certificates for each Automate node then provide different private key for each of Automate node.

`Public key for Automate`
: If you have a custom certificate for Automate node provide your public for Automate, If you have a custom certificates for each Automate node then provide different public key for each of Automate node.

## Private/Public Key For Chef Server

`Private key for Chef Server`
: If you have a custom certificate for Chef Infra Server node provide your private for Chef Infra Server. If you have a custom certificates for each Chef Infra Server node then provide different private key for each of Chef Infra Server node.

`Public key for Chef Server`
: If you have a custom certificate for Chef Infra Server node provide your public for Chef Infra Server. If you have a custom certificates for each Chef Infra Server node then provide different public key for each of Chef Infra Server node.

## OpenSearch Certificate and Private/Public Key

`Root CA for Open Search`
: In case of have custom certificates for Open Search node provide root certificates.

`Admin Key certificate for Open Search`
: In case of have custom certificates for Open Search node provide admin key certificates.

`Admin certificate for Open Search`
: In case of have custom certificates for Open Search node provide admin certificates.

`Private key for Open Search`
: If you have a custom certificate for Open Search node provide your private for Open Search, If you have a custom certificates for each Open Search node then provide different private key for each of Open Search node.

`Public key for Open Search`
: If you have a custom certificate for Open Search node provide your public for Open Search, If you have a custom certificates for each Open Search node then provide different public key for each of Open Search node.

## PostgreSQL Certificate and Private/Public Key

`Root CA for Postgresql`
: In case of have custom certificates for Postgresql node provide root certificates.

`Private key for Postgresql`
: If you have a custom certificate for Postgresql node provide your private for Postgresql, If you have a custom certificates for each Postgresql node then provide different private key for each of Postgresql node.

`Public key for Postgresql`
: If you have a custom certificate for Postgresql node provide your public for Postgresql, If you have a custom certificates for each Postgresql node then provide different public key for each of Postgresql node.

## AWS Deployment

Settings required for AWS deployment.

`VPC ID`
: VPC ID in which you want to create cluster.

`Private subnet ids`
: Three private subnets are required to create cluster.

`Public subnet ids`
: If you want to have public load balancer then, Three public subnets are required to create cluster.

`Instance type`
: Instance type to create cluster.

`EBS volume size`
: The EBS volume size.

`EBS volume type`
: Default is `gp3`.

`EBS volume IOPS`
: It should be based on your load needs.

`ssh key pair name`
: SSH key pair name on AWS. For example, `my-key`.

`Region`
: AWS region to create cluster.

`AMI Id`
: AWS AMI ID for specific region to create cluster of particular AMI.

`AWS profile name`. AWS profile name configured in .aws/credentials, Skip this if the IAM role is configured on the bastion host.

## External Databases

Settings for AWS-managed or customer-managed databases.

### OpenSearch

`Opensearch domain name`
: Opensearch domain name deployed on AWS or customer environment.

`Opensearch domain url`
: For AWS managed provide domain URL without port and protocol.

  For example, `opensearch.example.com`.

: For customer managed OpenSearch provide domain URL along with port.

  For example, `opensearch.example.com:9200`.

`Opensearch user name`
: Username to login to OpenSearch.

`Opensearch user passwords`
: Password to login to OpenSearch.

`Opensearch root-ca`
: SSL root certificates to connect with OpenSearch.

  If you have AWS managed databases we have option to use default AWS certificates - - If using default certificates then no need to provide root certificates.

### PostgreSQL

`PostgreSQL URL and port`
: Postgresql URL along with port.

  For example, `postgresql.example.com:5432`.

`PostgreSQL super username`
: Superuser username to login to PostgreSQL.

`PostgreSQL super user password`
: Superuser password to login to PostgreSQL.

`PostgreSQL database username`
: Database username to login to PostgreSQL.

`PostgreSQL database user password`
: Database password to login to PostgreSQL.

### AWS OpenSearch

`Aws OpenSearch snapshot arn`
: Snapshot arn is required to take a backup from AWS OpenSearch

`Aws OpenSearch snapshot user accesskey`
: Snapshot user accesskey is required to take a backup from AWS OpenSearch

`Aws OpenSearch snapshot secret key`
: Snapshot user accesskey is required to take a backup from AWS OpenSearch. Refer to the [Enabling OpenSearch Backup Restore](/automate/managed_services/#enabling-opensearch-backup-restore) section, to create them and get their values.

## Backup

If backup is configured during deployment, set the following settings.

`Bucket name`
: Object storage bucket name. In case of AWS deployment bucket will be created if not exist in AWS.

`Access Key`
: S3 access key.

`Secret Key`
: S3 secret key.

`Endpoint`
: Endpoint of object storage.

`Region`
: S3 Bucket region.

`Mount path`
: For file system or EFS backup, provide the mount path of the backup directory.

`Location`
: For Google Cloud Storage backup, `gcs`.

  For S3 backup (AWS S3, MinIO, non-AWS S3), `s3`.

`Google Service Account File`
: For Google Cloud Storage provide the credentials file path. For example, `/path/to/file/test.json`.
