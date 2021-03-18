+++
title = "Chef Infra Server"

weight = 20
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Server"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef_infra_server.md Chef Infra Server"
    weight = 20
+++

## Overview

The __Chef Infra Server__ page shows all the Chef Infra Servers connected to Chef Automate. The __Chef Infra Server__ user interface can be used to view and manage all the Chef Infra Server Objects like:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

The __Chef Infra Server__ acts as a hub for configuration data. The Chef Infra Server stores **cookbooks**, the policies that are applied to **nodes**, and metadata that describes each registered node that is being managed by Chef Infra Client. Nodes use Chef Infra Client to ask the Chef Infra Server for configuration details, such as recipes, templates, and file distributions. Chef Infra Client then does as much of the configuration work as possible on the nodes themselves (and not on the Chef Infra Server). This scalable approach distributes the configuration effort throughout the **organization**.

## Connect to the Chef Infra Server

To start making use out of the Chef Infra Server, firstly deploy and run [Chef Automate](https://docs.chef.io/automate/install/#installation-guide) instance.

**Note:** Chef Automate automatically displays your connected Chef Infra Servers.

Login to Chef Automate using the credentials. Once logged in, the page looks like as shown below:

![Chef Automate](/images/automate/chef-automate-on-chef-infra-page.png)

Click on **Infrastructure** located at the top bar of the page. Refer to the image below:

![Chef Automate with Highlighted Infrastructure](/images/automate/chef-automate-infrastructure.png)

Click on the **Chef Servers** located at the left panel of the page. The page looks like as shown below:

![Chef Infra Server](/images/automate/chef-server-page.png)

The above image is the **Chef Server** page which contains all the Chef Infra Servers connected to the Chef Automate. The Chef Infra Servers listed are the ones that are already configured and added to the Chef Automate. To create a new Infra Server click [here](https://docs.chef.io/automate/infra_server/).

To add the created Chef Server to the Infrastructure, click on **Add Chef Server** button. Refer to the image below:

![Add Chef Server](/images/automate/add-chef-server-button.png)

Clicking on the **Add Chef Server** button opens a popup menu as shown below:

![Add Chef Server Popup Menu](/images/automate/add-chef-server-popup-menu.png)

Mention the correct Name, FQDN, and IP Address of the system. Click on **Add Chef Server**. The desired server gets added to the Chef Infra Server.

**Note:** If the mentioned FQDN and IP Address is incorrect, the box displays an error as shown below:

![Add Chef Server Popup Menu](/images/automate/add-chef-server-popup-menu-with-error.png)

Click on the listed Servers to view the details of the specific **Chef Server**.

## Connect to the Chef Infra Server Organization

Selecting a specific added Chef Server opens the list of organizations in the Server. Refer to the image below:

![Chef Infra Server Organization](/images/automate/chef-server-organization.png)

The Organization in the Chef server listed are the ones which are already created in that specific server. To create a new Organization in the Server click [here](https://docs.chef.io/automate/infra_server/#set-up-the-chef-infra-server).

To add the previously created organization to the Infrastructure, click on the **Add Chef Organization** button. Refer to the image below:

![Add Chef Organization](/images/automate/add-chef-organization-button.png)

Clicking on the **Add Chef Organization** button opens a popup menu as shown below:

![Add Chef Organization Popup Menu](/images/automate/add-chef-organization-popup-menu.png)

Add the Name, Projects, Admin User, and Admin Key. Always use the exact details that were provided at the time of setting up **Knife**. Copy the contents of the `~/.chef/USER.pem` file and past it into the **Admin Key** field. Once done click **Add Chef Organization**. The desired organization will be added to the Chef Server.

Click on any Chef Organization to view the **Objects**.

## Access Chef Infra Server Objects

The **Chef Infra Server Objects** can be managed from the Chef Automate. In Automate Infra views, following objects can be managed:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

Refer to the image below:

![Chef Infra Server Objects](/images/automate/chef-infra-server-objects.png)

### Cookbooks

A [cookbook](https://docs.chef.io/cookbooks/) is the fundamental unit of configuration and policy distribution. include recipes and other files, components, or directories.

### Roles

[Roles](https://docs.chef.io/roles/) let you define patterns and processes that exist across nodes in an organization as belonging to a single job function. Each role consists of zero (or more) attributes and a run-list.

### Environments

An [environment](https://docs.chef.io/environments/) can be used to map an organization’s real-life workflow to what can be configured and managed when using Chef Infra. This mapping is accomplished by setting attributes and pinning cookbooks at the environment level.

### Data Bags

[Data bags](https://docs.chef.io/data_bags/) store global variables as JSON data. Data bags are indexed for searching and can be loaded by a cookbook or accessed during a search.

### Clients

Chef Infra Clients provide secure API access to the Chef Infra Server.

## Troubleshoot

While fetching any object like cookbooks or any, if it raises the `Could not get cookbooks: organization 'no-org' does not exist` error, the provided organization does not exist on Chef Infra Server. The error looks like as shown below:

![Could not get cookbooks: Organization](/images/automate/could-not-get-cookbooks-organization.png)

Create the Chef Organization using the knife command, `knife opc org create` or the Chef Infra Server CLI `chef-server-ctl org-create`. Then add the **Name**, **Projects**, **Admin User**, and **Admin Key** to fetch the objects.
