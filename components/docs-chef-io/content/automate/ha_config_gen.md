+++
title = "Autoamte HA Config Generation"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Automate Config Generation"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_config_gen.md Automate Config Generation"
    weight = 200
+++
{{< warning >}}
 {{% automate/ha-warn %}}
{{< /warning >}}

## Command Usage
    ```bash 
    sudo -- sh -c "
    chef-automate config gen config.toml
    "
    ```
    
`config.toml` is the file where all the generated config will be saved at the end, you can choose to not provide this then the output will be shown to stdout.

Refer the fields below to generate Chef Automate High Availability (HA) configuration using `chef-automate config gen` command

- we need to have `Chef Automate HA` as a topology for HA deployments
- `On-Premise` to deploy on customer created Automate HA cluster
- `Aws` to deploy in AWS enviornment, Cluster will be created by Automate HA
-  `Deployment` is a config type, we may have different type of configs in future.
-  `ssh user name` user name to ssh to cluster instances 
-  `ssh group name` group name which is associated with ssh user
-  `ssh port no` port to do ssh, default is 22 incase you have different port then provide the ssh port no
-  `ssh key file path` ssh key file path, same will be used to ssh to cluster instances
-  `Automate FQDN` automate FQDN name Example `chefautomate.example.com`.
-  `Automate FQDN ARN` for Aws deployment ARN name is requied for Automate FQDN domain.
-  `Automate FQDN Root Certificate` ssl root certificate for Automate FQDN domain.
-  `Automate Admin Password` Admin password to login to automate dashboard.
-  `Chef Server FQDN` automate FQDN name Example `chefserver.example.com`.
-  `Chef Server FQDN ARN` for Aws deployment ARN name is requied for Chef Serversss FQDN domain.
-  `Chef Server FQDN Root Certificate` ssl root certificate for Chef Server FQDN domain.
-  `Automate node count` number of nodes we want to keep for automate, in case of On-Premise deployment need to provide IP Address for all nodes
-  `Chef Server node count` number of nodes we want to keep for Chef Server, in case of On-Premise deployment need to provide IP Address for all nodes
-  `Opensearch node count` number of nodes we want to keep for Opensearch, in case of On-Premise deployment need to provide IP Address for all nodes
-  `Postgresql node count` number of nodes we want to keep for Postgresql, in case of On-Premise deployment need to provide IP Address for all nodes
-  
-  `Private key for Automate` In case to have custom certificate for Automate node provide your private for Automate, If you have custom certifiates for each Automate node then provide different private key for each of Automate node
-  `Public key for Automate` In case to have custom certificate for Automate node provide your public for Automate, If you have custom certifiates for each Automate node then provide different public key for each of Automate node

-  `Private key for Chef Server` In case to have custom certificate for Chef Server node provide your private for Chef Server, If you have custom certifiates for each Chef Server node then provide different private key for each of Chef Server node
-  `Public key for Chef Server` In case to have custom certificate for Chef Server node provide your public for Chef Server, If you have custom certifiates for each Chef Server node then provide different public key for each of Chef Server node

-  `Root CA for Open Search` In case of have custom certificates for Open Search node provide root certificates
-  `Admin Key certificate for Open Search` In case of have custom certificates for Open Search node provide admin key certificates
-  `Admin certificate for Open Search` In case of have custom certificates for Open Search node provide admin certificates
-  `Private key for Open Search` In case to have custom certificate for Open Search node provide your private for Open Search, If you have custom certifiates for each Open Search node then provide different private key for each of Open Search node
-  `Public key for Open Search` In case to have custom certificate for Open Search node provide your public for Open Search, If you have custom certifiates for each Open Search node then provide different public key for each of Open Search node

-  `Root CA for Postgresql` In case of have custom certificates for Postgresql node provide root certificates
-  `Private key for Postgresql` In case to have custom certificate for Postgresql node provide your private for Postgresql, If you have custom certifiates for each Postgresql node then provide different private key for each of Postgresql node
-  `Public key for Postgresql` In case to have custom certificate for Postgresql node provide your public for Postgresql, If you have custom certifiates for each Postgresql node then provide different public key for each of Postgresql node

- Details required for AWS deployment
  -  `VPC ID` VPC Id in which you want to create cluster
  -  `Private subnet ids` three private subnets are required to create cluster
  -  `Public subnet ids` in case you want to have public load balancer then, Three public subnets are required to create cluster
  -  `Instance type` instance type to create cluster
  -  `EBS volume size` it should be based on your load needs.
  -  `EBS volume type` default is `gp3`, change based on your need
  -  `EBS volume IOPS` it should be based  on your load needs.
  -  `ssh key pair name` ssh key pair name on AWS
  -  `Region` AWS region to create cluster
  -  `AMI Id` AWS AMI Id for specific region to create cluster of particular AMI
  -  `AWS profile name`. AWS profile name configured in .aws/credentials, Skip this if the IAM role is configured on the bastion host

In Case of AWS managed or Customer managed databases below fields will be required
-  `Opensearch domain name` Opensearch domain name deployed on AWS or customer enviroment
-  `Opensearch domain url` For AWS managed provide domain url without port and protocol Example: `opensearch.example.com`, and for customer managed opensearch provide domain url along with port Example `opensearch.example.com:9200`
-  `Opensearch user name`, username to login to opensearch
-  `Opensearch user passwords` password to login to opensearch
-  `Opensearch root-ca` SSL root certifiates to connect with opensearch, In Case of AWS managed databases we have option to use default aws certificates - - If using default certificates then no need to provide root certificates.
   -  `Postgresql URL and port` Postgresql url along with port Example: `postgresql.example.com:5432`
   -  `PostgreSQL super username` master username to login to postgresql
   -  `PostgreSQL super user password` master password to login to postgresql
   -  `PostgreSQL database username` database username to login to postgresql
   -  `PostgreSQL database user password` database password to login to postgresql
   -  `Aws OpenSearch snapshot arn` snapshot arn is required to take a backup from aws OpenSearch
   -  `Aws OpenSearch snapshot user accesskey` snapshot user accesskey is required to take a backup from aws OpenSearch
   -   `Aws OpenSearch snapshot secret key` snapshot user accesskey is required to take a backup from aws OpenSearch, 
       -   Please refer (/automate/managed_services/#enabling-opensearch-backup-restore) to create them and get their values.
- If configuring backup at the time of deployment the  we many need following fields
    - `Bucket name` for object storage or AWS S3 type of backup provide bucket name, for AWS deployment bucket will be created if not exist in AWS
    - `Access Key` S3 access key
    - `Secret Key` S3 secret key
    - `Endpoint` for object storage provide endpoint of object storage
    - `Region` for S3 provide region
    - `Mount path` in case of file system/efs backup provide mount path of backup directory.
    - 