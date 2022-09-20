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
{{% automate/4x-warn %}}
{{< /warning >}}

In this section, we'll discuss the steps to deploy Chef Automate HA on-premise machines or on existing VM's. The steps are as follows:

## Install Chef Automate HA

### Prerequisites

- All VM's or Machines are up and running.
- OS Root Volume (/) must be at least 40 GB
- TMP space (/var/tmp) must be at least 5GB
- Separate Hab volume (/hab) provisioned at least 100 GB, for opensearch node `/hab` volume will be more based on the data retention policy.
- A Common user has access to all machines.
- This common user should have sudo privileges.
- This common user uses same SSH Private Key file to access all machines.
- Key-based SSH for the provisioning user for all the machine for HA-Deployment.
- We do not support passphrase for Private Key authentication.
- LoadBalancers are setup according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
- DNS is configured to redirect `chefautomate.example.com` to Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, then these records should be added to `/etc/hosts` in all the machines including Bastion:

```bash
sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
```

- If the instance is **RedHat**, set SElinux config `enforcing` to `permissive` in all the nodes.\
  SSH to each node then run:

```bash
sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config
```

### Run these steps on Bastion Host Machine

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

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha existing_infra
   "
   ```

   {{< note >}} 
   Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.
   {{< /note >}}

   Note: If Airgapped Bastion machine is different, then transfer Bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the Airgapped Bastion Machine using `scp` command. \
   After transfering, in Airgapped Bastion, run below commands:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Move the Chef Automate CLI to `/usr/bin`.
   cp -f chef-automate /usr/bin/chef-automate

   #Generate init config and then generate init config for existing infra structure
   chef-automate init-config-ha existing_infra
   "
   ```

2. Update Config with relevant data. Click [here](/automate/ha_onprim_deployment_procedure/#sample-config) for sample config

   ```bash
   vi config.toml
   ```

   - Add No. of machines for each Service: Chef Automate, Chef Infra Server, Postgresql, OpenSearch
   - Add IP address of each machine in relevant service section, multiple IP's shoud be in double quotes (`"`) and separated with comma (`,`). Example: `["10.0.0.101","10,0.0.102"]`
      - If we want to use same machine for OpenSearch and Postgresql then provide same IP for both the config fields. Which means overall there will 3 machines or VM's running both OpenSearch and Postgresql. A reduced performance should be expected with this. Minimum 3 VM's or Machines will be used for Both OpenSearch and Postgresql running together on all 3 machines.
      - Also, you can use same machines for Chef Automate and Chef Infra Server. Which means overall there will be 2 machines or VM's running both Chef Automate and Chef Infra Server. A reduced performance should be expected with this. Minimum 2 VM's or Machines will be used by both Chef Automate and Chef Infra Server running together on both 2 machines.
      - Thus, overall minimum machines needed will be 5.
   - Give `ssh_user` which has access to all the machines. Example: `ubuntu`
   - Give `ssh_port` in case your AMI is running on custom ssh port, default will be 22.
   - Give `ssh_key_file` path, this key should have access to all the Machines or VM's.
   - `sudo_password` is only meant to switch to sudo user. If you have configured password for sudo user, please provide it here.
   - We support only private key authentication.
   - Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VM's. Example: `chefautomate.example.com`
   - Set the `admin_password` to what you want to use to login to Chef Automate, when you open up `chefautomate.example.com` in the Browser, for the username `admin`.

3. Continue with the deployment after updating config:

   ```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Print data in the config
   cat config.toml

   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib

   #After Deployment is done successfully. Check status of Chef Automate HA services
   chef-automate status
   ```

   Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

### Sample config

{{< note >}}

- Assuming 10+1 nodes (1 bastion, 2 for automate UI, 2 for Chef-server, 3 for Postgresql, 3 for Opensearch)

{{< /note >}}

```config
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.
[architecture.existing_infra]
ssh_user = ""
# private ssh key file path to access instances
# Eg.: ssh_user = "~/.ssh/A2HA.pem"
ssh_key_file = ""
ssh_port = "22"
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "existing_nodes"
workspace_path = "/hab/a2_deploy_workspace"
# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"
# ============== EC2 Nodes Config ======================
[automate.config]
# Automate Load Balancer FQDN eg.: "chefautomate.example.com"
fqdn = ""
instance_count = "2"
config_file = "configs/automate.toml"
[chef_server.config]
instance_count = "2"
[opensearch.config]
instance_count = "3"
[postgresql.config]
instance_count = "3"
[existing_infra.config]
## === INPUT NEEDED ===
# provide comma seperated ip address of nodes, like ["192.0.0.1", "192.0.0.2", "192.0.0.2"]
# No of ip address should be same as No of instance_count count mentioned above in
# automate.config, chef_server.config, opensearch.config and postgresql.config
automate_private_ips = ["A.B.C.D","D.E.F.G"]
chef_server_private_ips = ["I.J.K.L","M.N.O.P"]
opensearch_private_ips = ["A1.A2.A3.A4","B1.B2.B3.B4","C1.C2.C3.C4"]
postgresql_private_ips = ["D1.D2.D3.D4","E1.E2.E3.E4","F1.F2.F3.F4"]
```

### Minimum changes to be made

- Give `ssh_user` which has access to all the machines. Eg: `ubuntu`, `centos`, `ec2-user`
- Give `ssh_key_file` path, this key should have access to all the Machines or VM's. Eg: `~/.ssh/id_rsa`, `/home/ubuntu/key.pem`
- Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VM's. Eg: `chefautomate.example.com`
- `automate_private_ips` Eg: ["192.0.0.1"]
- `chef_server_private_ips` Eg: ["192.0.1.1"]
- `opensearch_private_ips` Eg: ["192.0.2.1", "192.0.2.2", "192.0.2.2"]
- `postgresql_private_ips` Eg: ["192.0.3.1", "192.0.3.2", "192.0.3.2"]

### How To Add More Nodes to the On-Prem Deployment

- Open the `config.toml` at bastion node
- change the `instance_count` value, as explained in below example.
- Add the `Ip address` for the respetive node at the end, as explained in the example.
- Make sure that all the necesssary port and fire wall setting are align in the new node.

For example : Add new Automate node to the existing deployed cluster.
  | Old Config | => | New Config |
  | :--- | :--- | :--- |
  | [automate.config] <br/ > instance_count = "1" <br/ > [existing_infra.config] <br/ > automate_private_ips = ["10.0.1.0"] |  | [automate.config] <br/ > instance_count = "2" <br/ > [existing_infra.config] <br/ > automate_private_ips = ["10.0.1.0","10.0.2.0"] |

- Trigger the deployment command again from the bastion node.
- In below deploy command,`latest.aib` will replace with your airgap bundle name which is running on the current cluster.
  
  ```sh
    chef-automate deploy config.toml --airgap-bundle latest.aib
  ```

- Above process can be done for `chef-server`, `postgresql` and `opensearch` cluster as well
- In case of Deployment failed please refer the troubleshoot document [here](/automate/ha_onprim_deployment_procedure/#Troubleshooting).

### How To Remove Any Nodes From Frontend Cluster (On-Prem Deployment)

{{< warning >}}
 We do not recommend removal of any node from backend cluster, but replacement of node is recommended. For replacement of a node please refer [this](/automate/ha_onprim_deployment_procedure/#How-to-Replace-Node-in-Automate-HA-Cluster).
{{< /warning >}}

- Open the `config.toml` at bastion node.
- change the `instance_count` value, as explained in below example.
- Remove the `Ip address` for the respective node, as explained in the example.

For example : Remove Automate node to the existing deployed cluster.
  | Old Config | => | New Config |
  | :--- | :--- | :--- |
  | [automate.config] <br/ > instance_count = "3" <br/ > [existing_infra.config] <br/ > automate_private_ips = ["10.0.1.0","10.0.2.0","10.0.3.0"] |  |[automate.config] <br/ > instance_count = "2" <br/ > [existing_infra.config] <br/ > automate_private_ips = ["10.0.1.0","10.0.3.0"] |
  
- Trigger the deployment command again from the bastion node.
- In below deploy command,`latest.aib` will replace with your airgap bundle name which is running on the current cluster.
  
  ```sh
    chef-automate deploy config.toml --airgap-bundle latest.aib
  ```

- Above process can be done for `chef-server` and `automate`.
- In case of Deployment failed please refer the troubleshoot document [here](/automate/ha_onprim_deployment_procedure/#Troubleshooting).

### How to Replace Node in Automate HA Cluster

- Open the `config.toml` at bastion node.
- Remove the `Ip Address` of unhealth node as explained in below example.
- Add the new `Ip address` value, as explained in below example.
- Make sure that all the necessary  port and fire wall setting are align in the new node.

 For example : Remove node to the existing deployed Postgres cluster.
 In the example we are replacing the node `10.0.7.0` with `10.0.9.0`.

  | Old Config | => | New Config |
  | :--- | :--- | :--- |
  | [existing_infra.config] <br/ > postgresql_private_ips = ["10.0.6.0","10.0.7.0","10.0.8.0"] |  | [existing_infra.config] <br/ > postgresql_private_ips = ["10.0.6.0","10.0.9.0","10.0.8.0"] |
  
- Run the below command from the bastion node.

  ```sh
  cd /hab/a2_deploy_workspace/terraform
  for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep module); do terraform taint $x; done
  cd -
  ```

- Run the `deploy` command to Add new node into the cluster.
- In below deploy command,`latest.aib` will replace with your airgap bundle name which is running on the current cluster.
  
  ```sh
    chef-automate deploy config.toml --airgap-bundle latest.aib
  ```

### Troubleshooting

#### Failure in adding nodes

  ```bash
  Error: Upload failed: scp: /var/automate-ha: Permission denied
  ```

- **Resolution** : Execute the below command.
  
  ```sh
  cd /hab/a2_deploy_workspace/terraform
  for x in $(terraform state list -state=/hab/a2_deploy_workspace/terraform/terraform.tfstate | grep module); do terraform taint $x; done
  cd -
   ```

- Once the module's tainted, run the `deploy` command again.`latest.aib` should be replaced with your airgap bundle name which is running on the current cluster

  ```sh
     chef-automate deploy config.toml --airgap-bundle latest.aib
  ```
