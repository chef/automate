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

1. Before starting, switch to sudo:

   ```bash
   sudo su -
   ```

2. Download Chef Automate CLI

   ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

3. Download Airgapped Bundle \
   Download latest Bundle with this:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
   ```

   Download specific version bundle with this, example version: 4.0.91:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/4.0.91.aib -o automate-4.0.91.aib
   ```

4. If Airgapped Bastion machine is different, then transfer Bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the Airgapped Bastion Machine using `scp` command. \
   After transfering, in Airgapped Bastion, swtich to sudo:

   ```bash
   sudo su -
   ```

   Move the Chef Automate CLI to `/usr/bin` by running below command:

   ```bash
   cp -f chef-automate /usr/bin/chef-automate
   ```

5. Generate init config \
   Then generate init config for existing infra structure:

   ```bash
   chef-automate init-config-ha existing_infra
   ```

6. Update Config with relevant data

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
   - Give `ssh_key_file` path, this key should have access to all the Machines or VM's
   - Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VM's. Example: `chefautomate.example.com`
   - Set the `admin_password` to what you want to use to login to Chef Automate, when you open up `chefautomate.example.com` in the Browser, for the username `admin`.

7. Confirm all the data in the config is correct:

   ```bash
   cat config.toml
   ```

8. Run Deploy Command \
   Deploy `latest.aib` with set `config.toml`

   ```bash
   chef-automate deploy config.toml --airgap-bundle latest.aib
   ```

   If deploying specific version of Chef Automate, example: Deploy `automate-4.0.91.aib` with set `config.toml`

   ```bash
   chef-automate deploy config.toml --airgap-bundle automate-4.0.91.aib
   ```

9. After Deployment is done successfully. \
   Check status of Chef Automate HA services:

   ```bash
   chef-automate status
   ```

   Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).
