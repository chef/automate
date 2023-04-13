+++
title = "Chef Infra Server/Chef Backend to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Che Infra Server/Chef Backend to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_chef_backend_to_automate_ha.md Chef Backend to Automate HA"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}
- Customers using only **Standalone Chef Infra Server** or **Chef Backend** are advised to follow this migration guidance. Customers using **Chef Manage** or **Private Chef Supermarket** with Chef Backend should not migrate with this.
- Also, for the customers using a standalone Chef Infra Server, cookbooks should be in the database but not in either the file system or S3.
- Automate HA do not support the super market authentication with chef-server users credentials. 
- Post Migration Customer can not login with chef-server users to Supermarket. 
{{< /warning >}}

This page explains the procedure to migrate the existing Standalone Chef Infra Server or Chef Backend data to the newly deployed Chef Automate HA. This migration involves two steps:

-   Back up the data from an existing Chef Infra Server or Chef Backend via `knife-ec-backup`.
-   Restore the backed-up data to the newly deployed Chef Automate HA environment via `knife-ec-restore`.

Take backup using the `knife-ec-backup` utility and move the backup folder to the newly deployed Chef Server. Later, restore using the same utility. The backup migrates all the cookbooks, users, data-bags, policies, and organizations.

`knife-ec-backup` utility backups and restores the data in an Enterprise Chef Server installation, preserving the data in an intermediate, editable text format. It is similar to the knife download and knife upload commands and uses the same underlying libraries. It includes workarounds for unsupported objects by the tools and various Server API deficiencies. The goal is to improve knife download, knife upload, and the Chef Infra Server API to deprecate the tool.

{{< note >}}

- The migration procedure is tested on Chef Server version 14+.
- The migration procedure is possible above Chef Backend version 2.1.0.

{{< /note >}}

## Backup the Existing Chef Infra Server or Chef Backend Data

1.   Execute below command to install Habitat:

```cmd
curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh \ | sudo bash
```
 
2.   Execute the below command to install the habitat package for `knife-ec-backup`

```cmd
    hab pkg install chef/knife-ec-backup
```

3.   Execute the below command to generate a knife tidy server report to examine the stale node, data etc.
     In this command:
-   `pivotal` is the name of the user
-   `path of pivotal` is the path where the user's pem file is stored.
-   `node-threshold NUM_DAYS` is the maximum number of days since the last checking before a node is considered stale.

```cmd
    hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s <chef server URL> -u <pivotal> -k <path of pivotal>
```
For Example:

```cmd
    hab pkg exec chef/knife-ec-backup knife tidy server report --node-threshold 60 -s https://chef.io -u pivotal -k /etc/opscode/pivotal.pem
```

4.   Execute the below command to initiate a backup of your Chef Server data. 
    In this command:

- `with-user-sql` is required to handle user passwords and ensure user-specific association groups that are not duplicate.

- `--with-key-sql` is to handle cases where customers have users with multiple pem keys associated with their user or clients. The current chef-server API only dumps the default key. Sometimes, users will generate and assign additional keys to give additional users access to an account but still be able to lock them out later without removing everyone's access.

```cmd
    hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s <chef server
```

For example:

```cmd
    hab pkg exec chef/knife-ec-backup knife ec backup backup_$(date '+%Y%m%d%H%M%s') --webui-key /etc/opscode/webui_priv.pem -s https://chef.io`.
```

-  Execute the below command to clean unused data from reports. This is an optional steps

``` bash
    hab pkg exec chef/knife-ec-backup knife tidy server clean --backup-path /path/to/an-ec-backup
```

5.   Execute the below command to copy the backup directory to the Automate HA Chef Server.
```cmd
     scp -i /path/to/key backup\_$(date '+%Y%m%d%H%M%s') user@host:/home/user
```

## Restore Backed Up Data to Chef Automate HA

-   Execute the below command to install the habitat package for `knife-ec-backup`

```cmd
    hab pkg install chef/knife-ec-backup
```

-   Execute the below command to restore the backup.

```cmd
    hab pkg exec chef/knife-ec-backup knife ec restore /home/centos/backup\_2021061013191623331154 -yes --concurrency 1 --webui-key /hab/svc/automate-cs-oc-erchef/data/webui\_priv.pem --purge -c /hab/pkgs/chef/chef-server-ctl/*/*/omnibus-ctl/spec/fixtures/pivotal.rb
```

## In place Migration (Chef Backend to Automate HA)

As part of this scenario, customer will migrate from chef-backend (5 machines) to Automate HA in-place, i.e. Automate HA will be deployed in those 5 machines only where Chef-backend is running. One extra bastion node will be required which will be managing the deployment of Automate HA on the chef backend infrastructure.

{{< note >}}

This will require downtime, so plan accordingly. A reduced performance should be expected with this. 

{{< /note >}}

{{< note >}}

- Need to setup your workstation based on newly created Automate-HA's chef-server. Only needed if you have setup the workstation earlier. 
- This inplace migration works only when cookbook are stored at database. This do not support use-case, where cookbooks are stored at filesystem. 
- Take the backup of the system to avoid the data loss.
{{< /note >}}

1. ssh to all the backend nodes of chef-backend and run

```cmd
    chef-backend-ctl stop
```

2. ssh to all frontend nodes of chef-backend and run

```cmd
    chef-server-ctl stop
```

3. Create one bastion machine under the same network space.

4. ssh to bastion machine and download chef-automate cli

```cmd
https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip
```

5. Extract downloaded zip file

6. Create a airgap bundle using command

```cmd
    ./chef-automate airgap bundle create 
```

7. Geneate `config.toml` file using command

```cmd 
    ./chef-automate init-config-ha existing_infra 
```

8. Edit `config.toml` and add the following:

- Update the `instance_count`  
- fqdn : load balance url, which points to frondend node.
- keys : ssh username and private keys
- Make sure to provide Chef backend's frontend server IPs for Automate HA Chef Automate and Chef Server.
- Make sure to provide Chef backend's backend server IPs for Automate HA Postgres and OpenSearch machines.
- Sample configuration, please modify according to your needs.

```cmd
[architecture.existing_infra]
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "existing_nodes"
workspace_path = "/hab/a2_deploy_workspace"
ssh_user = "myusername"
ssh_port = "22"
ssh_key_file = "~/.ssh/mykey.pem"

# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"

[automate.config]
# admin_password = ""
# automate load balancer fqdn IP or path
fqdn = "chef.example.com"
instance_count = "2"
# teams_port = ""
config_file = "configs/automate.toml"

[chef_server.config]
instance_count = "2"

[opensearch.config]
instance_count = "3"

[postgresql.config]
instance_count = "3"

[existing_infra.config]
automate_private_ips = ["10.0.1.0","10.0.2.0"]
chef_server_private_ips = ["10.0.1.0","10.0.2.0"]
opensearch_private_ips = ["10.0.3.0","10.0.4.0","10.0.5.0"]
postgresql_private_ips = ["10.0.3.0","10.0.4.0","10.0.5.0"]
```

9. Deploy using

```cmd
./chef-automate deploy config.toml <airgapped bundle name>
```

10. Clean up the old packages from chef-backend (like Elasticsearch and postgres)

11. [Doc: Restore Backed Up Data to Chef Automate HA](/automate/ha_chef_backend_to_automate_ha/#restore-backed-up-data-to-chef-automate-ha)


## Using Automate HA for Chef-Backend user

1. Download and Install chef-workstation 
    From Bastion machine or local machine install chef-workstation
    https://www.chef.io/downloads/tools/workstation
    
2. Set up workstation
    ```bash 
    chef generate repo chef-repo 
    ```
    ```bash 
    cd  chef-repo 
    ```
    ```bash 
    knife configure 
    ```

    provide chef-server FQDN of Automate HA Chef-Server
    
    Example :-
    https://demo-chef-server.com/organizations/demo-org

    ```bash 
        knife configure
        Please enter the chef server URL: [https://ip-10-1-0-52.ap-southeast-1.compute.internal/organizations/myorg] https://demo-chef-server.com/organizations/demo-org
        Please enter an existing username or clientname for the API: [ubuntu] org-user
        *****

        You must place your client key in:
            /root/.chef/org-user.pem
        Before running commands with Knife

        *****
        Knife configuration file written to /root/.chef/credentials
    ```

3. Ssl fetch command
    ```bash 
    knife ssl fetch 
    ```
4. Ssl check command
    ```bash 
    knife ssl check 
    ```

5. Create role command 
    ```bash 
    knife role create abc --disable-editing 
    ```

6. Download cookbook 
    ```bash 
    knife supermarket download line 
    knife supermarket download apache2
    ```


7. Upload cookbook
    ```bash 
    knife cookbook upload line --cookbook-path cookbooks 
    ```

8. Bootstrap as node
    ```bash 
        knife bootstrap <public_ip_node>  -i <key_to_connect_node> -U ubuntu -N test1 --sudo 
    ```
    Example :- 
    ```bash 
        knife bootstrap 15.207.98.155 -i  ~/.ssh/ssh-key.pem -U ubuntu -N node1 --sudo 
        knife bootstrap 3.110.103.65 -i ~/.ssh/ssh-key.pem -U ubuntu -N node2 --sudo
    ```