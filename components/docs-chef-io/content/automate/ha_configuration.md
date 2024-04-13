+++
title = "High Availability Configuration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Configuration"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha_configuration.md High Availability Configuration"
    weight = 12
+++

A Chef Automate configuration file contains all the settings for deploying Automate in a high availability configuration. This includes settings for backing up data; connecting to OpenSeach, Chef Infra Server, and PostgreSQL nodes; and configuring Automate deployed on AWS.

## File format

Chef Automate uses a [TOML v0.4.0 file](https://toml.io/en/v0.4.0) for configuration.

## Create the configuration file

Use the `chef-automate` CLI to generate a new configuration file with default values.

```sh
chef-automate init-config-ha
```

To create a default configuration for deployment on AWS, use the `aws` argument.

```sh
chef-automate init-config-ha aws
```

To create a default configuration file on a bastion host that is part of an existing high availability deployment, use the `existing_infra` argument.

```sh
chef-automate init-config-ha existing_infra
```

## Use the configuration file

Use the `chef-automate CLI on view, patch, and reset a configuration file.

You can use the examples below to configure your TOML file.

## Infrastructure

The following sets the basic configuration of Chef Automate’s infrastructure in a high availability deployment.

### Parameters

#### secrets_key_file

Type: string

Default: /hab/a2_deploy_workspace/secrets.key

Some words that describe this.

#### secrets_store_file

Type: string
Default: none
Description text.

#### architecture

Type: string
Default: none
Description text.

#### workspace_path

Type: string
Default: none
Description text.

#### ssh_user

Type: string
Default: none
Description text.

#### ssh_port

Type: string
Default: none
Description text.

#### ssh_key_file

Type: string
Default: none
Description text.

#### sudo_password

Type: string
Default: none
Description text.

#### backup_mount

Type: string
Default: /mnt/automate_backups
Description text.

Do not modify this value.

#### backup_config

Type: string
Default: None
Allowed values: file_system, object_storage.

The type of storage for backing up Chef Automate data.

If this value is set to object_storage, set the object storage settings.

### Example

```ruby
###### Automate Infrastructure ##########################################################
# The following sets the basic configuration of Chef Automate's infrastructure.
# See docs.chef.io/automate/config_toml#infrastructure for a full description of each property.
# See https://docs.aws.amazon.com/s3/ for documentation on AWS S3.
#########################################################################################

###### Parameters #######################################################################
# secrets_key_file
#     The file path to the .....
#     Default: "/hab/a2_deploy_workspace/secrets.key"
# secrets_store_file
#
# architecture
#
# workspace_path
#
# ssh_user
#
# ssh_port
#
# ssh_key_file
#
# sudo_password
#     The configured sudo password for the account.
#     This is only required if the root user has a sudo password set.
# backup_mount
#     The file path ...
#     Default: "/mnt/automate_backups"
#     Do not modify this value.
# backup_config
#     Whether to back up data to file system storage or object storage.
#     Allowed values are: "file_system" and "object_storage".
#     If set to "object_storage", add the Object Storage configuration section to your
#     config file.
#########################################################################################

[architecture.existing_infra]
secrets_key_file = ""
secrets_store_file = ""
architecture = ""
workspace_path = ""
ssh_user = ""
ssh_port = ""
ssh_key_file = ""
sudo_password = ""
backup_mount = "/mnt/automate_backups"
backup_config = ""
```

## Object Storage

### Parameters

#### bucket_name

Type: string
Default: none
The name of the AWS S3 bucket to back up data to.

#### access_key

Type: string
Default: none
The AWS IAM access key for accessing the S3 bucket used to back up data to.

#### secret_key

Type: string
Default: none
Other param description.

#### endpoint

Type: string
Default: none
Other param description.

#### region

Type: string
Default: none
Other param description.

### Example

```ruby
###### Object Storage ###################################################################
# The following settings configure Chef Automate to back up data on AWS S3.
# See docs.chef.io/automate/config_toml#object-storage for a full description of each property.
# See https://docs.aws.amazon.com/s3/ for documentation on AWS S3.
# You must set "backup_config" to "object_storage" in the infrastructure settings.
#########################################################################################

###### Parameters #######################################################################
# bucket_name
#      The name of the AWS S3 bucket to back up data to.
# access_key
#      The AWS IAM access key for accessing the S3 bucket used to back up data to.
# other_param
#      Other param description.
#########################################################################################

[object_storage.config]
bucket_name = ""
access_key = ""
secret_key = ""
endpoint = ""
region = ""
```

## Chef Automate

## Chef Infra Server

## PostgreSQL

## OpenSearch

## AWS

## External database

## Event Gateway
