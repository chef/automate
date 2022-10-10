+++
title = "Node Bootstrapping"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Node Bootstrapping"
    identifier = "automate/deploy_high_availability/ha_node_bootstraping.md Node Bootstrapping"
    parent = "automate/deploy_high_availability"
    weight = 95
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

A node is any physical, virtual, or cloud device configured and maintained by an instance of Chef Infra Client. Bootstrapping installs Chef Infra Client on a target system so it can run as a client and sets the node up to communicate with a Chef Infra Server.

To bootstrap a node, run `knife bootstrap` in the workstation.

## Knife Bootstrap

The knife bootstrap command is a common way to install Chef Infra Client on a node. The default for this approach assumes that a node can access the Chef website so that it may download the Chef Infra Client package from that location.

The Chef Infra Client installer will detect the version of the operating system and then install the appropriate Chef Infra Client version using a single command to install Chef Infra Client and all of its dependencies, including an embedded version of Ruby, OpenSSL, parsers, libraries, and command-line utilities.

The Chef Infra Client installer puts everything into a unique directory (/opt/chef/) so that Chef Infra Client will not interfere with other applications that may be running on the target machine. Once installed, Chef Infra Client requires a few more configuration steps before performing its first Chef Infra Client run on a node.

## Bootstrap a Node

### Create Users and Organization

1. ssh in the bastion host and follow the below steps.

2. ssh in chef_server instance.

`sudo chef-automate ssh --hostname chef_server`

3. Ensure all the front-end instances are up and running.

`sudo chef-automate status`

4. Create a User.

Syntax:

```bash
sudo chef-server-ctl user-create <username> <First Name> <Last Name> <Email ID> <password> -f <path and file name to store user's pem file>
```

For Example:

```bash
sudo chef-server-ctl user-create johndoe John Doe john.doe@example.com John@123 -f ./johndoe.pem
```

Created users can be listed using `sudo chef-server-ctl user-list`

5. Create an organization.

Syntax:

```bash
sudo chef-server-ctl org-create <org name> '<org display name>' --association_user <username> -f <path and file name to store org's pem file>
```

For Example:

```bash
sudo chef-server-ctl org-create new_org 'New Organization' --association_user johndoe -f ./new_org.pem
```

Created organization can be listed using `sudo chef-server-ctl org-list`

7. Copy the `pem` files and save them to a safe location.

### Workstation Setup

1. To set up the workstation on your machine, follow the steps given below:

-   Install the latest version of chef Workstation on the ubuntu system.

```bash
wget https://packages.chef.io/files/stable/chef-workstation/21.7.524/ubuntu/20.04/chef-workstation_21.7.524-1_amd64.deb
```

-   To install the same:

```bash
dpkg -i chef-workstation_21.7.524-1_amd64.deb
```

-   Verify the installation using following command:

```bash
chef -v
```

Click [here](https://docs.chef.io/workstation/install_workstation/) for any additional information.

2. Generate chef-repo using `chef generate repo chef-repo`. Click [here]https://docs.chef.io/workstation/getting_started/ to know more.

3. Paste `pem` files of user and organization inside `/root/.chef/`. For example: `Eg.: /root/.chef/johndoe.pem , /root/.chef/new_org.pem`

4. Paste ssh key of node which you want to bootstrap inside `/root/.ssh/<ssh_key_of_node>`.

5. Edit Credentials file `vi /root/.chef/credentials` or run `knife configure` to configure the credentials.

6. Provide the name of user-created in chef_server, correct path of pem file of user and chef server URL (or associated DNS), and organization name.

Once configured, `/root/.chef/credentials` will look like as shown below:

```bash
  [default]
  client_name = "<name_of_user>"
  client_key = "/root/.chef/<pem_file_of_user>"
  chef_server_url = "https://demo-server.saas.chef.io/organizations/<name_of_organization>/"
```

7. Run the following command:

```bash
knife ssh fetch
knife ssl check
```

{{< note >}} `knife ssl check` might throw certificate error in which case, configuration can be done using DNS attached to Chef Server URL {{< /note >}}

The above command will fetch certificate details, save them to the trusted_cert folder in **/root/.chef/**, and verify the same.

8. Run the bootstrap command.

`knife bootstrap <Public_ip> -i ~/<pem_file_of_node> -U ubuntu -N <name_of_node> --sudo`

-   **Public IP:** IP address of the node which we are bootstrapping.

-   **pem_file_of_node:** `pem` file of node which we have saved at `/root/.ssh/<pem_file_of_node>`.

-   **name_of_node:** You can provide any name to your node.

For example: `knife bootstrap 3.124.**.** -i ~/.ssh/rsa.pem -U ubuntu -N johndoe`

## Troubleshoot

If `knife bootstrap` throws permission denied or cannot create directory error, add the following configuration in `/root/.chef/credentials` and then run the bootstrap command as shown in _Step No. 8_.

```bash
[default.knife]
ssh_user = 'ubuntu' # this would have been knife[:ssh_user] in your config.rb
aws_profile = 'default'
use_sudo = true
```
