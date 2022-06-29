## Node Bootstrapping

A node is any physical, virtual, or cloud device that is configured and maintained by an instance of Chef Infra Client. Bootstrapping installs Chef Infra Client on a target system so that it can run as a client and sets the node up to communicate with a Chef Infra Server

To bootstrap a node we can run `knife bootstrap` in workstation.

## knife bootstrap

The knife bootstrap command is a common way to install Chef Infra Client on a node. The default for this approach assumes that a node can access the Chef website so that it may download the Chef Infra Client package from that location.

The Chef Infra Client installer will detect the version of the operating system, and then install the appropriate Chef Infra Client version using a single command to install Chef Infra Client and all of its dependencies, including an embedded version of Ruby, OpenSSL, parsers, libraries, and command line utilities.

The Chef Infra Client installer puts everything into a unique directory (/opt/chef/) so that Chef Infra Client will not interfere with other applications that may be running on the target machine. Once installed, Chef Infra Client requires a few more configuration steps before it can perform its first Chef Infra Client run on a node.

## Bootstraping a node

### Create Users and organization.

1. ssh in the bastion host and follow the below steps.

2. ssh in chef_server instance.

`sudo chef-automate ssh --hostname chef_server`

3. Ensure all the front end instances are up and running.

`sudo chef-automate status`

4. Create organization

Syntax:

```
  sudo chef-server-ctl org-create <org name> '<org display name>' -f <path and file name to store org's pem file>
```

Example:

```
  sudo chef-server-ctl org-create new_org 'New Organization' -f ./new_org.pem
```

5. Create User.

Syntax:

```
sudo chef-server-ctl user-create <username> <First Name> <Last Name> <Email ID> <password> -f <path and file name to store user's pem file>
```

Example:

```
sudo chef-server-ctl user-create johndoe John Doe john.doe@example.com John@123 -f ./johndoe.pem
```

6. Once the user is created we need to add the user to the organization. Execute the below command for the same:

```
sudo knife opc org user add <org_name> <username> --admin
```

Example:

```
sudo knife opc org user add new_org johndoe --admin
```

--admin will provide the admin privileges to the user.

7. Copy both the pem files and save it to safe location.

### Workstation Setup

1. To set up the workstation on your machine follow theses steps

-   Install the latest version of chef Workstation on ubuntu system.

    ```
    wget https://packages.chef.io/files/stable/chef-workstation/21.7.524/ubuntu/20.04/chef-workstation_21.7.524-1_amd64.deb
    ```

-   For installing the same

    ```
    dpkg -i chef-workstation_21.7.524-1_amd64.deb
    ```

-   Verify installation by

    ```
    chef -v
    ```

-   For additional information please refer https://docs.chef.io/workstation/install_workstation/

2. Generate chef-repo using `chef generate repo chef-repo`

-   For additional information refer https://docs.chef.io/workstation/getting_started/

4. Paste pem file of user inside `/root/.chef/<pem_file_of_user>`
   `Eg.: /root/.chef/johndoe.pem`

5. Paste pem file of node which we want to bootstrap inside `/root/.ssh/<pem_file_of_node>`

6. Edit Credentials file `vi /root/.chef/credentials` or run `knife configure` to configure the credentials.

Provide name of user created in chef_server, correct path of pem file of user and chef server URL (or associated DNS) and organization name.

Once configured, `/root/.chef/credentials` would look like:

```
  [default]
  client_name = "<name_of_user>"
  client_key = "/root/.chef/<pem_file_of_user>"
  chef_server_url = "https://demo-server.saas.chef.io/organizations/<name_of_organization>/"
```

7. Run following command

```
knife ssh fetch
kinfe ssl check
```

This will fetch certificate details and save it to trusted_cert folder in /root/.chef/ and verify the same.

7. Run bootstrap command.

`knife bootstrap <Public_ip> -i ~/<pem_file_of_node> -U ubuntu -N <name_of_node> --sudo`

> Public IP - IP address of node which we are bootstrapping

> pem_file_of_node - pem file of node which we have saved at `/root/.ssh/<pem_file_of_node>`

> name_of_node - you can provide any name to your node.

`Eg: knife bootstrap 3.124.**.** -i ~/.ssh/rsa.pem -U ubuntu -N johndoe`

### Troubleshoot

If `knife bootstrap` throws premission denied or cannot create directory error, added the following configureation in `/root/.chef/credentials` and then run the boostrap command as shown in point number 7

```
[default.knife]
ssh_user = 'ubuntu' # this would have been knife[:ssh_user] in your config.rb
aws_profile = 'default'
use_sudo = true
```
