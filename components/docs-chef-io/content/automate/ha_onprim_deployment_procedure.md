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

This section will discuss the steps to deploy Chef Automate HA on-premise machines or on existing VMs. The steps are as follows:

## Install Chef Automate HA

### Prerequisites

- All VMs or Machines are up and running.
- The Root Volume of the Operating System should not be less than **40 GB**.
- The TMP space (/var/tmp) should not be less than **5GB**.
- The Separate Hab's volume (/hab) provisioned should be at least 100 GB. The `/hab` volume depends on the data retention policy for the OpenSearch node.
- A typical user has access to all machines, should have **sudo privileges**, and can use the same **SSH Private Key** file to access all devices.
- Key-based SSH for the provisioning user for all the machines for HA-Deployment.
- LoadBalancers are setup according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
- DNS is configured to redirect `chefautomate.example.com`, and `chefinfraserver.example.com` to Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, these records should be added to `/etc/hosts` in all the machines, including Bastion:

```bash
sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
```

- If the instance is **RedHat**, set SElinux config `enforcing` to `permissive` in all the nodes. SSH to each node by running the following command:

```bash
sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config
```

### Steps for Bastion Host Machine

1. Before you start, switch to sudo:

   ```bash
   sudo su -
   ```

2. Download Chef Automate CLI:

   ```bash
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate
   ```

3. Download the latest Airgapped Bundle using the following command:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o latest.aib
   ```

   Download a specific version bundle using the comman below. For example: `4.0.91`:

   ```bash
   curl https://packages.chef.io/airgap_bundle/current/automate/4.0.91.aib -o automate-4.0.91.aib
   ```

4. If the Airgapped Bastion machine is different, transfer the Bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the Airgapped Bastion Machine using the `scp` command. After transferring, in Airgapped Bastion, switch to sudo:

   ```bash
   sudo su -
   ```

   Move the Chef Automate CLI to `/usr/bin` by running the below command:

   ```bash
   cp -f chef-automate /usr/bin/chef-automate
   ```

5. Generate the `init` config for existing infrastructure:

   ```bash
   chef-automate init-config-ha existing_infra
   ```

6. Update Config with relevant data:

   ```bash
   vi config.toml
   ```

   - Add No. of machines for each Service: Chef Automate, Chef Infra Server, Postgresql, and OpenSearch.
   - Add the IP address of each machine in the relevant service section. Multiple IPs should be in double quotes (`"`) and separated with a comma (`,`). Example: `["10.0.0.101","10,0.0.102"]`
      - If we want to use the same machine for OpenSearch and Postgresql, provide the same IP for both the config fields. There will be three machines or VMs running in both OpenSearch and Postgresql. A reduced performance should be expected with this.
      - Also, you can use the same machines for Chef Automate and Chef Infra Server. This means there will be two machines or VMs running both Chef Automate and Chef Infra Server. A reduced performance should be expected with this. Minimum 2 VM or Machines will be used by both Chef Automate and Chef Infra Server running together on both two machines.
      - Thus, the overall minimum number of machines needed will be five.
   - Give `ssh_user`, which has access to all the machines. Example: `ubuntu`.
   - Give `ssh_port` if your AMI runs on a custom ssh port. The default will be 22.
   - Give `ssh_key_file` path. This key should have access to all the Machines or VMs.
   - Give `fqdn` as the DNS entry of Chef Automate, which LoadBalancer redirects to Chef Automate Machines or VMs. Example: `chefautomate.example.com`.
   - Set the `admin_password` to what you want to use to login to Chef Automate when you open up `chefautomate.example.com` in the Browser, for the username `admin`.

7. Confirm all the data in the config is correct:

   ```bash
   cat config.toml
   ```

8. Run deploy command `latest.aib` with set `config.toml`:

   ```bash
   chef-automate deploy config.toml --airgap-bundle latest.aib
   ```

   If deploying a specific version of Chef Automate, for example: Deploy `automate-4.0.91.aib` with set `config.toml`

   ```bash
   chef-automate deploy config.toml --airgap-bundle automate-4.0.91.aib
   ```

9. After Deployment is done successfully. Check the status of Chef Automate HA services:

   ```bash
   chef-automate status
   ```

   Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).
