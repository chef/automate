+++
title = "Install Workflow"
description = ""
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "workflow"
    weight = 20 
+++

Workflow is a legacy feature for Chef Automate, which was designed for managing changes to both infrastructure and application code.

{{< warning >}}
Workflow is only available in Chef Automate for existing users. If you are not already using Workflow, but are interested in the solution it offers, please contact your sales or success representative for support with continuous integration pipelines.
{{< /warning >}}

## Prerequisites

Create a system which is accessible from your Automate 2.0 environment via SSH.
This will have a minimum of three machines:

* Automate With Workflow - 8GB RAM recommended
* Chef Server - 8GB RAM recommended
* Workflow Runner - 20GB disk space recommended

## Installation

### Build an Automate server

Create an Automate server following the instructions in the:
[Quickstart Demo]({{< ref "quickstart.md" >}}) or [Installation Guide]({{< ref "install.md" >}})

On the Automate server:

1. Deploy the Workflow server:

    ```shell
    chef-automate deploy --enable-workflow
    ```

    You will need to add `--skip-preflight` if you have already deployed Automate

2. Create a workflow enterprise:

    ```text
    workflow-ctl create-enterprise my-enterprise-name --ssh-pub-key-file=/hab/svc/automate-workflow-server/var/etc/builder_key.pub
    ```

### Build a Standalone Chef Server

Create a standalone Chef server following the [standalone server installation](https://docs.chef.io/install_server.html#standalone).

Then, on the Chef server:

1. Create a 'workflow' user on the Chef Server:

    ```shell
    sudo chef-server-ctl user-create workflow workflow user ops@some.domain.com workflow
    ```

2. Creating a user in previous step displays a private key to stdout. Copy this content and and save it on the **Chef Automate** server as: `/hab/svc/automate-workflow-server/var/etc/delivery.pem`

3. If this is a new chef-server, create an organization on the chef server with:

    ```shell
    `sudo chef-server-ctl org-create workflow workflow
    ```

4. Add the 'workflow' user as an admin to the Chef Server organization that you created when setting up your Chef Server:

    ```shell
    sudo chef-server-ctl org-user-add my-chef-org workflow --admin
    ```

### Configure the Chef Automate Server

1. Set up your Chef Automate server to communicate with your Chef Server by creating a `workflow.toml`:

    ```toml
    [workflow.v1.sys.chef_server]
    url = "https://my.chef.server.com/organizations/my-chef-organization"
    web_ui_url = "https://my.chef.server.com/"
    chef_user = "workflow"
    ```

    The FQDN saved in the `workflow.toml` on the Chef Automate Server must match the FQDN of the Chef Server (find it by entering `hostname --fqdn` into your Chef Server terminal) or you will have SSL certificate failures later in this process.

    For example, if running `hostname --fqdn` from your Automate server command line returns `automate-test`, then that hostname will need to be in the local dns configuration on your workstation.

    For example, `/etc/hosts` would contain a line like:

    ```text
      192.168.0.20    automate-test
    ```

1. Apply the configuration:

    ```shell
    chef-automate config patch workflow.toml
    ```

### Login to the Web UI

1. Navigate to `https://automate-test` on your workstation
2. Login as _admin_, using the password from the  `automate-credentials.toml` file that was created on your Automate server with `chef-automate deploy`

## Workflow

Access Workflow by opening the _Client Runs_ tab and then selecting **Workflow** on the left navigation panel.

![](/images/docs/workflow-side.png)

Selecting **Workflow** opens the legacy Workflow screen.

![](/images/docs/workflow-legacy.png)

### Create an Organization

1. From the Workflow UI, select **Workflow Orgs** on the left
1. Select **New Workflow Org**
1. Enter an organization name, for example, "testing", and select **Save & Close**

![](/images/docs/workflow-org.png)

### Set a Workflow Admin SSH Key

1. From the Workflow UI, select the _Admin_ tab on top
1. Select the **edit** button for the admin user
1. Enter a first and last name, and an email address
1. Paste your ssh public key into the **SSH KEY** field
1. Select **Save & Close**

![](/images/docs/workflow-ssh.png)

### Configure Runners

#### On the Runner

1. Create another VM that is also accessible from your Automate 2.0 environment.
2. Create a new user on the VM, for example, "workflow"

    ```shell
      sudo useradd workflow -G sudo --create-home
    ```

3. Let new user to use sudo without entering a password, by putting the new user into the `sudo` group and ensuring this line exists at the end of `/etc/sudoers`

    ```shell
    %sudo  ALL=(ALL) NOPASSWD:ALL
    ```

4. Copy the SSH public key from the `/hab/svc/automate-workflow-server/var/etc/builder_key.pub` file that was made during the enterprise creation step on the Automate server to the `/home/workflow/.ssh/authorized_keys` file on the new VM.
5. Confirm the hostname and IP addresses of the Chef Automate server and the Chef servers in `/etc/hosts`

    For example, `/etc/hosts` would contain lines like:

    ```text
      192.168.0.20    automate-test
      192.168.1.100   chef-server
    ```

#### On the Automate 2 server

1. From the command line, run:

    ```text
    workflow-ctl install-runner my.vm.hostname.or.ip.address workflow -i /hab/svc/automate-workflow-server/var/etc/builder_key
    ```

2. Open Automate in a web browser, then select the _Client Runs_ tab, and finally, select Workflow on the menu on the left.
3. Select **Runners** from the navbar
4. Select **Manage Runners** from the page
5. You should see your newly installed runner, select the **TEST** button

### Creating a Pipeline

1. Download the latest stable release [ChefDK](https://downloads.chef.io/chefdk).
2. Create a new directory, for example, `delivery-test`, and move into that directory:

    ```shell
    mkdir delivery-test
    cd delivery-test
    ```

3. Initialize the repository with:

    ```shell
    git init
    ```

4. Setup the pipeline, using values from your own system:

    ```shell
    delivery setup --ent=demo --org=testing --user=admin --server=automate-test --a2-mode
    ```

    1. Replace "demo" with the name created in `workflow-ctl create-enterprise`
    1. Replace "testing" with the name of the organization you created in Workflow
    1. The user should remain _admin_

5. Confirm that you're logged into the Workflow UI in your web browser
6. Run `delivery init --a2-mode`
7. It will prompt for a token and open your web browser. Select copy on the page that displays the token.
Return to your shell and paste the token.

## Troubleshooting

### workflow-ctl

For clarity, we renamed `automate-ctl` command `workflow-ctl`. Resolve the following error message:

```text
deployment-service.default(O):time="2018-12-10T21:30:03Z"
level=error msg="Converge failed" error="failed to binlink
command "automate-ctl" in pkg "chef/automate-workflow-ser
ver/2.8.31 20181207185527" into /bin\nxxx\nxxx
'automate-ctl' was not found under any 'PATH' directories
in the chef automate-workflow-server/2.8.31/20181207185527
package\nxxx\n: exit status 1"
```

by making a manual binlink to the new `workflow-ctl` binary with:

`$ hab pkg binlink chef/automate-workflow-server workflow-ctl -f`

### Public Key Error

If `delivery init --a2-mode` results in:

```text
Chef Delivery
Creating Delivery project...
  Skipping: Delivery project named wftest already exists.
Setting up the 'delivery' git remote...
  Skipping: The delivery git remote is up-to-date.
  (ssh://admin@automate@a2-dev.test:8989/automate/rupert
  /wftest).
Pushing initial git history...
\The authenticity of host '[a2-dev.test]:8989
([127.0.0.1]:8989)' can't be established.
RSA key fingerprint is
SHA256:L+6QhMbxVD3I8Z3yCN1sUKbKbWGHEGjjBmyP71g0WdA.
Are you sure you want to continue connecting (yes/no)?|-\-
Git command failed!
STDOUT:
STDERR: Warning: Permanently added '[a2-dev.test]:8989'
(RSA) to the list of known hosts.
admin@automate@a2-dev.test: Permission denied (publickey).
fatal: Could not read from remote repository.

Please make sure you have the correct access rights
and the repository exists.
```

You may need to add the private key associated with the ssh public key for the Workflow admin in the UI:

```shell
ssh-add -K ~/.ssh/<ssh private key>
```

### Known Issues

* Authentication times out after three minutes if you don't keep a browser window open on Chef Automate.
