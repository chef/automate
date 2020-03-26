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

{{% warning %}}

Chef Automate will not deploy the Chef Infra Server add-ons Chef Manage
and Push Jobs Server.

{{% /warning %}}

{{% warning %}}

Supermarket cannot authenticate users on Chef Infra Server that is deployed
with Chef Automate.

{{% /warning %}}

Use Chef Automate to install Chef Infra Server either for a single-host installation that contains both Chef Infra Server and Chef Automate, or for a standalone Chef Infra Server instance.
See the [Chef Infra Server documentation](https://docs.chef.io/server_overview/) for instructions and guidance on using and managing your Chef Infra Server.

## System Requirements and Prerequisites

Before beginning your installation, check the [System Requirements]({{< relref "system-requirements.md" >}}) for Chef Automate, and ensure that the `chef-automate` command line tool installed.

To download the `chef-automate` command line tool, run the following command in your command line interface:

```shell
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

## How to Install Chef Automate and Chef Infra Server on the Same Host

Use either a command line interface or a configuration file to install Chef Automate and Chef Infra Server on the same host.
Installations require elevated privileges, so run the commands as the superuser or use `sudo` at the start of each command.

### Hardware Requirements for Single-Host Installation

For a single-host installation that contains Chef Infra Server and Chef Automate, we recommend the following memory and vCPU minimums:

* up to 200 managed nodes: 8GB RAM, 2 vCPUs
* between 200 and 500 managed nodes: 30GB RAM, 4 vCPUs
* between 500 and 5000 managed nodes: 61GB RAM, 8vCPUs

A single-host installation that contains Chef Infra Server and Chef Automate requires a `/hab` directory that contains 80 GB of disk space for software artifacts plus 2 MB of disk space per managed node.

### Command Line Install of Chef Automate and Infra Server

Install Chef Automate and Chef Infra Server on the same host with this command:

```shell
sudo chef-automate deploy --product automate --product infra-server
```

### Configuration File Install of Chef Automate and Infra Server

Installing Chef Automate and Chef Infra Server on the same host using a configuration file also requires the use of the Chef Automate CLI.
Installations require elevated privileges, so run the commands as the superuser or use `sudo` at the start of each command.

1. First, generate a skeleton configuration file by running this command:

    ```shell
      sudo chef-automate init-config
    ```

1. Add a stanza to the configuration file to deploy Chef Automate and Chef Infra Server:

    ```toml
      [deployment.v1.svc]
      products=["automate", "infra-server"]
    ```

1. Make any other configuration changes desired.

1. Run the `chef-automate deploy` command with your configuration file:

    ```shell
      sudo chef-automate deploy config.toml
    ```

## Install Standalone Chef Infra Server through Chef Automate

Use either a command line interface or a configuration file to install Chef Infra Server through Chef Automate.

Refer to the on Chef Infra Server [hardware requirements](https://docs.chef.io/install_server_pre/) for guidance on memory and number of CPUs.

### Command Line Install of Standalone Chef Infra Server

When Chef Automate deploys the Chef Infra Server, it automatically configures the Chef Infra Server to collect data to send to Chef Automate.
To deploy a standalone Chef Infra Server with Chef Automate, you must turn off data collection in the configuration.
Installations require elevated privileges, so run the commands as the superuser or use `sudo` at the start of each command.

1. First, generate a skeleton configuration file by running:

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
       sudo chef-automate deploy --product infra-server <configuration_file>
    ```

### Configuration File Install of Standalone Chef Infra Server

Installing Chef Infra Server through Chef Automate using a configuration file also requires the use of the Chef Automate CLI.
When Chef Automate deploys the Chef Infra Server, it automatically configures the Chef Infra Server to collect data to send to Chef Automate.
To deploy a standalone Chef Infra Server with Chef Automate, you must turn off data collection in the configuration.
Installations require elevated privileges, so run the commands as the superuser or use `sudo` at the start of each command.

1. First, generate a skeleton configuration file by running the following command:

    ```shell
      sudo chef-automate init-config
    ```

1. Add a stanza to the configuration file to deploy Chef Infra Server:

    ```toml
       [deployment.v1.svc]
       products=["infra-server"]

       # Disable Automate data collection as Automate will not be deployed
       [erchef.v1.sys.data_collector]
       enabled = false
    ```

1. Run the `chef-automate deploy` command with your configuration file (config.toml):

    ```shell
      sudo chef-automate deploy config.toml
    ```
