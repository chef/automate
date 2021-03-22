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

The _Chef Infra Server_ page (**Infrastructure > Chef Infra Server**) lets you connect existing Chef Infra Servers to Chef Automate, view all of the connected Chef Infra Servers, and manage all of the objects on your connected Chef Infra Servers.

The _Chef Infra Server_ acts as a hub for configuration data. The Chef Infra Server stores _cookbooks_, the policies that are applied to _nodes_, and metadata that describes each registered node that is being managed by Chef Infra Client.

Nodes use Chef Infra Client to ask the Chef Infra Server for configuration details, such as recipes, templates, and file distributions. Chef Infra Client then does as much of the configuration work as possible on the nodes themselves (and not on the Chef Infra Server).

This scalable approach distributes the configuration effort throughout the _organization_.

{{< figure src="/images/automate/chef-server-page.png" width="500" alt="Chef Infra Server Page">}}

The objects that you can manage from the Chef Infra Server are:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

## Connect Chef Infra Servers to Chef Automate

The _Chef Infra Server_ starts out with an empty list. Adding your Chef Infra Servers to Chef Automate populates the list.

To add existing Chef Servers to the Infrastructure, select **Add Chef Serve**, which and the name, FQDN, and IP address of your Chef Infra Server in the form:

{{< figure src="/images/automate/add-chef-server-popup-menu.png" width="500" alt="Add Chef Server Form">}}

Entering an invalid FQDN and IP Address results in an error:

{{< figure src="/images/automate/add-chef-server-popup-menu-with-error.png" width="500" alt="Add Chef Server Form">}}

Once you're finished, select **Add Chef Server** and you'll see your server in the list of Chef Infra Servers

## Connect a Chef Organization to a Chef Infra Server

Selecting a server from the list of organizations in the Server. Refer to the image below:

{{< figure src="/images/automate/chef-server-organization.png" width="500" alt="Chef Infra Server Organization">}}

The Organization in the Chef server listed are the ones which are already created in that specific server. To create a new Organization in the Server select [here](https://docs.chef.io/automate/infra_server/#set-up-the-chef-infra-server).

To add the previously created organization to the Infrastructure, select **Add Chef Organization** button. Selecting on the button opens a popup menu as shown below:

{{< figure src="/images/automate/add-chef-organization-popup-menu.png" width="500" alt="Add Chef Organization Form">}}

Add the Name, Projects, Admin User, and Admin Key. Always use the exact details that were provided at the time of setting up _Knife_. Copy the contents of the `~/.chef/USER.pem` file and paste it into the _Admin Key_ field. Once done select **Add Chef Organization** to add the Chef Infra Server.

Select any Chef Organization to view the _Objects_.

## Access Chef Infra Server Objects

The Chef Infra Server Objects can be managed from the Chef Automate. In Automate Infra views, following objects can be managed:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

Refer to the image below:

{{< figure src="/images/automate/chef-infra-server-objects.png" width="500" alt="Chef Infra Server Objects">}}

### Cookbooks

A [cookbook]({{< relref "cookbooks" >}}) is the fundamental unit of configuration and policy distribution. Include recipes and other files, components, or directories.

### Roles

[Roles]({{< relref "roles" >}}) let you define patterns and processes that exist across nodes in an organization as belonging to a single job function. Each role consists of zero (or more) attributes and a run-list.

### Environments

An [environment]({{< relref "environments" >}}) can be used to map an organization's real-life workflow to what can be configured and managed when using Chef Infra. This mapping is accomplished by setting attributes and pinning cookbooks at the environment level.

### Data Bags

[Data bags]({{< relref "data_bags" >}}) store global variables as JSON data. Data bags are indexed for searching and can be loaded by a cookbook or accessed during a search.

### Clients

Chef Infra Clients provide secure API access to the Chef Infra Server.

## Troubleshoot

While fetching any object like cookbooks or any, you might face an error `Could not get cookbooks: organization 'no-org' does not exist`, which means the provided organization does not exist on Chef Infra Server. The error looks like as shown below:

{{< figure src="/images/automate/could-not-get-cookbooks-organization.png" width="500" alt="Could not get cookbooks: Organization">}}

Create the Chef Organization using the knife command, `knife opc org create` or the Chef Infra Server CLI command, `chef-server-ctl org-create`. Then add the _Name_, _Projects_, _Admin User_, and _Admin Key_ to fetch the objects.
