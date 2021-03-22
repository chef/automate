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

The _Chef Infra Server_ page (**Infrastructure > Chef Servers**) lets you connect existing Chef Infra Servers to Chef Automate, view all of the connected Chef Infra Servers, and manage all of the objects on your connected Chef Infra Servers.

The _Chef Infra Server_ acts as a hub for configuration data. The Chef Infra Server stores _cookbooks_, the policies that are applied to _nodes_, and metadata that describes each registered node that is being managed by Chef Infra Client.

Nodes use Chef Infra Client to ask the Chef Infra Server for configuration details, such as recipes, templates, and file distributions. Chef Infra Client then does as much of the configuration work as possible on the nodes themselves (and not on the Chef Infra Server).

This scalable approach distributes the configuration effort throughout the organization.

{{< figure src="/images/automate/chef-server-page.png" alt="Chef Infra Server Page">}}

The objects that you can manage from the Chef Infra Server are:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

## Connect Chef Infra Servers to Chef Automate

The _Chef Infra Server_ panel starts out with an empty list of servers.

To add existing Chef Infra Servers to the Chef Automate infrastructure, select **Add Chef Server** which will request the name, FQDN, and IP address of your Chef Infra Server:

{{< figure src="/images/automate/add-chef-server-popup-menu.png" width="500" alt="Add Chef Server Form">}}

Chef Automate will warn you if you enter an invalid FQDN or IP address:

{{< figure src="/images/automate/add-chef-server-popup-menu-with-error.png" width="500" alt="Add Chef Server Form">}}

Once you are finished, select **Add Chef Server** and you will see your server in the list of Chef Infra Servers.

## Connect a Chef Organization to a Chef Infra Server

To see a list of existing [organizations]({{< relref "server_orgs" >}}) that are applied to a Chef Infra Server,
select a Chef Infra Server from the **Chef Servers** list, then select the **Orgs** tab. Refer to the image below:

{{< figure src="/images/automate/chef-server-organization.png" alt="Chef Infra Server Organization">}}

To create a new organization, see the documentation on [setting up a Chef Infra Server]({{< relref "infra_server#set-up-the-chef-infra-server" >}}).

To add an existing organization, select **Add Chef Organization** which will open a popup menu as shown below:

{{< figure src="/images/automate/add-chef-organization-popup-menu.png" width="500" alt="Add Chef Organization Form">}}

Enter the _Name_, _Projects_, _Admin User_, and _Admin Key_ fields using the same values that were provided when the organization was configured using _Knife_.
Copy the contents of the `~/.chef/USER.pem` file and paste it into the **Admin Key** field. Then select **Add Chef Organization** to add the organization to the Chef Infra Server.

## Access Chef Infra Server components

The following Chef Infra Server components for an organization can be managed using Chef Automate:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

To access these components for an organization, select **Chef Servers > Organizations** and then select an organization. Refer to the image below:

{{< figure src="/images/automate/chef-infra-server-objects.png" alt="Chef Infra Server Objects">}}

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

While fetching any objects like cookbooks, you might face an error `Could not get cookbooks: organization 'no-org' does not exist`, which means the provided organization does not exist on Chef Infra Server.

{{< figure src="/images/automate/could-not-get-cookbooks-organization.png" alt="Could not get cookbooks: Organization">}}

Create the organization using the knife command, `knife opc org create` or the Chef Infra Server CLI command, `chef-server-ctl org-create`, then add the _Name_, _Projects_, _Admin User_, and _Admin Key_ to fetch the objects.
