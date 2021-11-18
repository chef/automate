+++
title = "Chef Automate HA Deployment"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - Reference Architecture"
    parent = "automate/High_Availability"
    identifier = "automate/reference/ha_deploy.md Chef Automate HA Deployment"
    weight = 20
+++

!-- Chef gonna give storage calculator for customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page??

## Chef Automate High Availability (HA) Installation

This section explains the two deployment types to support Chef Automate HA in your network premises/ infrastructure.

1. Amazon Web Services (AWS) Deployment

   The standard terraform script deploys the cluster of automate, chef-server, elastic-search and postgress on AWS cloud. This script includes steps that creates VPC (Amazon Virtual Private Cloud), security-group, setting up the EC2 (Amazon Elastic Compute Cloud) and so on.

   Is this required?`

   AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS) and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage and content delivery services. Learn more @ <https://aws.amazon.com/what-is-cloud-computing/>.

2. Bare Metal Infrastructure Deployment (existing_node)

   A **Bare Metal computer** is generally one without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

   In cloud computing, a *bare-metal* server is a non-shared computer dedicated to one customer. It generally implies a non-virtual machine (VM) environment.

   The difference between bare metal servers and cloud servers is that cloud server is a virtual machine while the bare metal server is a physical machine identified within a data center.

   Bare Metal deployments are installations of operating systems to targets that either have no operating system installed, or must be re-installed without preserving any existing data or settings.

   You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

## Chef Automate Installation on Fresh Server

Both types of deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` command, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip` and press **Enter**. The command downloads the Chef Automate utility installer in .zip format.
- Type the `gunzip` command, `gunzip - > chef-automate && chmod +x chef-automate` and press **Enter**. The command installs the utility and provides the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/chef_automate_install.png" alt="Chef Automate Utility Installation">}}

## Deployment Procedure on Bare Infrastructure

### Pre-requisites

- Chef Automate Utility
- Servers provisioned and accessible through SSH from each other.
- List of Virtual Machines (VM) with public and private  numbers. Public IP address is not mandatory. 
- Create the HAB user --how? 
- Create following directories for all *Postgress* nodes using commands as listed:

  - sudo mkdir -p /mnt/automate_backups/postgresql/pg_dump/ 
  - sudo mkdir -p /mnt/automate_backups/postgresql/archive/ 
  - sudo chown -R hab:hab /mnt/automate_backups/ 
  
- All VMs must expose the port 22 for SSH. Yoy may need to open certain port across the VMs to establish the communication, which are:

   | Component                                | Port                    |

   | :--------------------------------------: | :---------------------: |
   | Habitat gossip (UDP)                     |     9638                |
   | Habitat http API                         |     9631                |
   | PostgreSQL                               |     5432                |
   | Pgleaderchk                              |     6432                | 
   | HaProxy                                  |     7432                | 
   | Elasticsearch (https)                    |     9200                |
   | Elasticsearch (transport)                |     9300                |
   | Kibana                                   |     5601                |
   | Automate,ES-Node                         |     22,443              |

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes:

1. Open **Command Prompt**.
2. Login as a **root** user by typing `sudo su -`.
3. Type the command, `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.

{{< figure src="/images/automate/chef_automate_configtoml" alt="Chef Automate HA Default Configuration File">}}

1. In `config.toml` file, specify on-premise IPs, list of IP address for the cluster separated by comma. 

1. In `config.toml` file, specify public IPs for the virtual machines. In case, you dont have them, provide private IPs. The `config.toml` configuration file generates with default settings.

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

{{< figure src="/images/automate/chef_automate_configtomldefault" alt="View Chef Automate HA Default Configuration">}}

1. Type the command, `./chef-automate deploy config.toml` and press **Enter**. This command creates deployment workspace (*/hab/a2_deploy_workspace*), downloads Habitat, and establish cluster provisioning in your workspace.

{{< figure src="/images/automate/chef_automate_provision" alt="View Chef Automate HA Provisioning">}}

1. Login as a root user by typing command, `sudo su`.

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

in config.toml: - I coudnt succeed
- specify the ssh username and the ssh key file path
- provide the number of node for the respective cluster. If we are using the automate cluster, we have to put behind the load balance. FQDN will be the load balancer URL. For PG,ES and chef-server we have to just maintain the cluster number.
- List of IP address for the cluster - there are options for private and public ip's. in case of we don't have public-ip for the vm's we can use the private ip
- Setup the secrets management key and any needed passwords. The default location for the secrets key and secret storage is set in the config file. - The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json

1. Type the command, `./chef-automate secrets init` and press **Enter**. This command generates a new secrets key, which is used to encrypt the secret store.

1. Type the command, `./chef-automate secrets set automate_admin_password` and press **Enter**. This command sets up the Chef Automate User interface (UI) password. ??

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

1. Type the command, `./scripts/credentials set postgresql –auto` and press **Enter**. This command rotates the credentials for Postgresql.

1. Type the command, `./scripts/credentials set elasticsearch –auto` and press **Enter**. This command rotates the credentials for ElasticSearch.

1. Type the command, `chef-automate test –full` and press **Enter**. This command runs smoke tests on the setup. ??

## Deployment Procedure on AWS Cloud

Follow the steps below to deploy Chef Automate HA on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
1. Login as a **root** user by typing `sudo su -`.
1. Type the command, `./chef-automate init-config-ha aws` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings and installs latest deployment package.

{{< figure src="/images/automate/chef_automate_configtoml" alt="Chef Automate HA Default Configuration File">}}

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

{{< figure src="/images/automate/chef_automate_configtomldefault" alt="View Chef Automate HA Default Configuration">}}

1. Type the command, `./chef-automate provision-infra config.toml` and press **Enter**. This command downloads Habitat, creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions the infrastructure on AWS.

{{< figure src="/images/automate/chef_automate_provision" alt="View Chef Automate HA Provisioning">}}

in config.toml: - I coudnt succeed
- specify the ssh username and the ssh key file path
- `chmod 400 /root/.ssh/id_rsa`
- provide the number of node for the respective cluster. If we are using the automate cluster, we have to put behind the load balance. FQDN will be the load balancer URL. For PG,ES and chef-server we have to just maintain the cluster number.
- type of instance (Before proceeding ahead, ensure that the instance type mentioned inside a2ha.rb file is supported in the region)
volume type, size and iops
aws region
ssh_key_pair_name (this is same key-pair name what we have used to provision the bastion ec2 machine). In Section-I we have define the ssh_key_file, this should point to the same key file.
Setup the secrets management key and any needed passwords. The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json


1. Type the command, `./chef-automate secrets init` and press **Enter**. This command generates a new secrets key, which is used to encrypt the secret store.

1. Type the command, `./chef-automate secrets set automate_admin_password` and press **Enter**. This command sets up the Chef Automate User interface (UI) password.

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

1. Type the command, `./scripts/credentials set postgresql –auto` and press **Enter**. This command rotates the credentials for Postgresql.

1. Type the command, `./scripts/credentials set elasticsearch –auto` and press **Enter**. This command rotates the credentials for ElasticSearch.

1. Type the command, `chef-automate test –full` and press **Enter**. This command runs smoke tests on the setup. ??

## Uninstall

Follow these steps to delete the *Terraform* and *HA Deployment Infrastructure*:

1. Navigate to the Chef Automate workspace folder where we have deployed HA. For example, `cd /hab/a2_deploy_workspace/terraform`.

1. Type the command, `terraform destroy` and press **Enter**. This command removes the terraform deployed at your workspace.

1. Type the command, `rm -rf /hab/a2_deploy_workspace` and press **Enter**.

(Optional) Create a uid/gid for hab user Habitat will automatically set a uid and gid for the hab user. If you need to override this, you can set uid/gid as per your requirements. If you don’t want to set this option, then leave it blank:

habitat_uid_gid=""

{{< note >}}

There is no uninstall procedure for Bare metal deployment type as the servers are with the customers as per their needs. You can clear the workspace as required.

{{< /note >}}
