+++
title = "Install Chef Infra Server With Automate"
description = "Use Chef Automate to install Chef Infra Server"
date = 2020-02-11T14:24:00-08:00
weight = 20
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "get_started"
    weight = 60
+++

Use Chef Automate to install Chef Infra Server either for a single-host installation that contains Chef Infra Server and Chef Automate, or for a standalone Chef Infra Server instance. See the [Chef Infra Server documentation](https://docs.chef.io/server_overview/) for
instructions and guidance on using and managing your Chef Infra Server.

## System requirements and prerequisites

Before beginning your installation, check the [System Requirements]({{< relref "system-requirements.md" >}}) for Automate, and ensure that the `chef-automate` command line tool is installed.

To download the `chef-automate` command line tool, run the following command in your command line interface:

```shell
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

## Install Chef Automate and Chef Infra Server on the same host

Use either a command line interface or a configuration file to install Chef Automate and Chef Infra Server on the same host. Run the commands as the superuser or use `sudo` at the start of each command.

### Hardware Requirements

For a single-host installation that contains Chef Infra Server and Chef Automate, we recommend the following memory and vCPU minimums:

* up to 200 managed nodes: 8GB RAM, 2 vCPUs
* between 200 and 500 managed nodes: 30GB RAM, 4 vCPUs
* between 500 and 5000 managed nodes: 61GB RAM, 8vCPUs

A single-host installation that contains Chef Infra Server and Chef Automate will require a `/hab` directory that contains 80 GB of disk space for software artifacts plus 2 MB of disk space per managed node.

### Specify Products From the Command Line

To install Chef Automate and Chef Infra Server on the same host, run this command:

```shell
sudo chef-automate deploy --product automate --product chef-server
```

### Specify Products Via the Configuration

To install Chef Automate and Chef Infra Server on the same host via configuration will still require a command line interface.

1. First, generate a skeleton configuration file by running this command:

    ```shell
      sudo chef-automate init-config
    ```

1. Add a stanza to the configuration file to deploy Automate and Chef Infra Server:

    ```toml
      [deployment.v1.svc]
      products=["automate", "chef-server"]
    ```

1. Make any other configuration changes desired.

1. Run the `chef-automate deploy` command with your configuration file:

    ```shell
      sudo chef-automate deploy config.toml
    ```

## Install standalone Chef Infra Server using Chef Automate

You can use either a command line option or a configuration file to install Chef Infra Server via Chef Automate. Run the commands as the superuser or use `sudo` at the start of each command.

### Hardware Requirements

For a standalone Chef Infra Server installation, see the [hardware requirements for Chef Infra Server](https://docs.chef.io/install_server_pre/) for guidance on memory and number of CPUs.

### Specify Product on the Command Line
When Automate deploys the Chef Infra Server, it automatically configures the Chef Infra
Server to collect data to send to Automate. To deploy standalone Chef Infra Server with Automate, you
must turn off data collection in the configuration.

1. Generate a skeleton configuration file by running:

    ```shell
       sudo chef-automate init-config
    ```

1. Add a stanza to the configuration file to disable Automate data collection:

    ```toml
       [erchef.v1.sys.data_collector]
       enabled = false
    ```

1. Use the configuration file to deploy Chef Infra Server by running the following command:

    ```shell
       sudo chef-automate deploy --product chef-server <configuration_file>
    ```

### Specify Product Via Configuration

1. Generate a skeleton configuration file by running the following command:

    ```shell
      sudo chef-automate init-config
    ```

1. Add a stanza to the configuration file to deploy Chef Infra Server:

    ```toml
       [deployment.v1.svc]
       products=["chef-server"]

       # Disable Automate data collection as Automate will not be deployed
       [erchef.v1.sys.data_collector]
       enabled = false
    ```

1. Run the `chef-automate deploy` command with your configuration file (config.toml):

    ```shell
      sudo chef-automate deploy config.toml
    ```
