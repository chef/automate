+++
title = "Migrate Bastion to New Machine"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Migrate Bastion to New Machine"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_migrate_bastion.md Migrate Bastion to New Machine"
    weight = 225
+++

{{< warning >}}
 {{% automate/ha-warn %}}
{{< /warning >}}

## Pre-requisite

- Have a working HA cluster with Bastion.
- New VM for a replacement Bastion with necessary hardware requirement for bastion (recommended to have same OS as the original bastion) - [AWS Deployment Prerequisites](/automate/ha_aws_deployment_prerequisites/#minimum-hardware-requirement)

{{< note >}} The replacement bastion should also be in the same VPC. To avoid any surprises, having the VM in the same subnet as the original Bastion with a similar configuration is suggested. {{< /note >}}

### Step-1: In Original/Old Bastion

1. Copy the artifacts directory to the user directory of the Replacement/New Bastion.

    ```bash
    scp -i <ssh_key_file> -r /hab/cache/artifacts <USER>@<NEW_BASTION_PUBLIC/PRIVATE_IP>:/home/<USER>/
    ```

1. Copy the  /hab/a2_deploy_workspace directory to the user directory of the Replacement/New Bastion.

    ```bash
    scp -i <ssh_key_file> -r /hab/a2_deploy_workspace <USER>@<NEW_BASTION_PUBLIC/PRIVATE_IP>:/home/<USER>/
    ```

1. Copy SSH_KEY_FILE used to connect to all frontend and backend nodes to the user directory of the Replacement/New Bastion.

    ```bash
    scp -i <ssh_key_file> <ssh_key_file> <USER>@<NEW_BASTION_PUBLIC/PRIVATE_IP>:/home/<USER>/
    ```

### Step-2: In Replacement/New Bastion

1. Install hab

    ```bash
    curl https://raw.githubusercontent.com/habitat-sh/habitat/master/components/hab/install.sh \ | sudo bash
    ```

#### Updating Permissions for Files

1. Give root permissions to the file inside artifacts and a2_deploy_workspace directory in the home directory of the Replacement/New Bastion machine.

    ```bash
    cd /home/<USER>/
    chown -RL root:root artifacts/*
    chown -RL root:root a2_deploy_workspace/*
    ```

1. Move those files into their respective places.

    ```bash
    mv -r artifacts/* /hab/cache/artifacts/
    mv -r a2_deploy_workspace/* /hab/a2_deploy_workspace/
    ```

1. Place the ssh_user_key in the same directory as it was in the Original/Old Bastion machine.

    Eg: If the key is `~/.ssh/id_rsa`, then in the Replacement/New Bastion also place it in `~/.ssh/id_rsa`

1. Give proper permission for the `ssh_key_file`.

    ```bash
    chmod 600 <ssh_key_file>
    ```

#### Install Packages

1. Identify the same CLI and Deployment package version used in the Original/Old Bastion machine.

    - For CLI

    ```bash
    ls -la /hab/cache/artifacts/*cli*
    ```

    - For deployment package.

    ```bash
    ls -la /hab/cache/artifacts/*automate-ha-deployment*
    ```

    {{< note >}}

    - If there is more than one CLI package available, use the latest one.
    - If more than one deployment package is available, identify the version used in the Original/Old Bastion machine and use the same version. Run ls -la /hab/ in the Original/Old Bastion machine to find the version.

    {{< /note >}}

1. Install CLI and add it to the `/bin`.

    ```bash
    hab pkg install -bf <path to .hart file for CLI>
    ```

1. Install the Automate Deployment Package.

    ```bash
    hab pkg install -bf <path to .hart file for deployment>
    ```

{{< note >}}

- Verify if chef-automate commands are working.
- Once the commands are verified, you can clean up the Old Bastion Original/Old Bastion.

{{< /note >}}
