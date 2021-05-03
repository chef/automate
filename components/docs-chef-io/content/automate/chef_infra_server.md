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

To view a list of existing [organizations]({{< relref "server_orgs" >}}) on a Chef Infra Server,
select a Chef Infra Server from the **Chef Servers** list, then select the **Organizations** tab, as shown below:

{{< figure src="/images/automate/chef-server-organization.png" alt="Chef Infra Server Organization">}}

To create a new organization, see the documentation on [setting up a Chef Infra Server]({{< relref "infra_server#set-up-the-chef-infra-server" >}}).

To add an existing organization, select **Add Chef Organization**, which opens a dialog box as shown below:

{{< figure src="/images/automate/add-chef-organization-popup-menu.png" width="500" alt="Add Chef Organization Form">}}

Enter the _Name_, _Projects_, _Admin User_, and _Admin Key_ fields using the same values that were provided when the organization was configured using _Knife_.
Copy the contents of the `~/.chef/USER.pem` file and paste it into the **Admin Key** field. Then select **Add Chef Organization** to add the organization to the Chef Infra Server.

## Access Chef Infra Server Components

The following Chef Infra Server components for an organization can be managed using Chef Automate:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

To access these components for an organization, select **Chef Servers > Organizations** and then select an organization. Refer to the image below:

{{< figure src="/images/automate/chef-infra-server-objects.png" alt="Chef Infra Server Objects">}}

### Cookbooks

A [cookbook]({{< relref "cookbooks" >}}) is the fundamental unit of configuration and policy distribution. A cookbook contains recipes and other files, components, or directories.

### Roles

[Roles]({{< relref "roles" >}}) let you define patterns and processes that exist across nodes in an organization as belonging to a single job function. Each role consists of zero (or more) attributes and a run-list.

The Chef Infra Server UI lets you:

- Create a role
- List out all the roles at one place.
- Search for a specific role from a list of roles.
- View the details of roles:
  - Run List
  - Attributes
- Delete a role

#### Create a Role

To create a new role, select **Create Role**:

{{< figure src="/images/automate/create-role-button.png" alt="Create Roles Button">}}

Selecting the **Create Role** button opens a dialog box. The dialog box contains four different sections: _Details_, _Run List_, _Default Attributes_ and _Override Attributes_. Enter the details in the possible sections and select **Create** to create a new role.

{{< figure src="/images/automate/create-role-popup.png" alt="Create Roles Dialog Box">}}

#### Search for a Role

Use the **Search Roles** bar to find the existing role from the list of roles. Entering the name of a role in the search box returns roles matching your search criteria.

{{< figure src="/images/automate/create-role-button.png" alt="Select Create Roles Button">}}

#### View Role Details

Select a specific role to view the details of that particular role. Its contains two tabs, _Details_ and _Attributes_.

{{< figure src="/images/automate/view-role-details.png" alt="View Role Details">}}

The _Details_ contains the Run List, whereas the attributes tab displays the *default* and the *override* attributes of the role.

{{< figure src="/images/automate/view-attributes-details.png" alt="View Attributes Details">}}

You can edit the *default* and *override* attributes by selecting the **Edit** option. It opens a popup window where you can make the changes and click **Save**.

#### Delete a Role

Chef Infra Server lets you delete the existing roles one at a time. To delete a role, select the ellipses icon and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-role.png" alt="Delete A Role">}}

### Environments

An [environment]({{< relref "environments" >}}) can be used to map an organization's real-life workflow to what can be configured and managed when using Chef Infra. This mapping is accomplished by setting attributes and pinning cookbooks at the environment level.

The Chef Infra Server UI lets you:

- Create an environment.
- Search for a specific environment from a list of environments.
- List out all the environments at one place.
- View the details of an environment.
- Edit an environment.
- Delete an environment.

#### Create an Environment

To create a new Environment, select **Create Environment**, as shown below:

{{< figure src="/images/automate/create-environment-button.png" alt="Create Environment Button">}}

Selecting the **Create Environment** button opens a dialog box. The dialog box contains four different sections: _Details_, _Constraints_, _Default Attributes_ and _Override Attributes_. Enter the details in the possible sections and select **Create** to create a new environment.

{{< figure src="/images/automate/create-environment-popup.png" alt="Create Environment Dialog Box">}}

#### Search for an Environment

Use the **Search environments** bar to find an existing environment from the list of environments.
Entering the name of an environment in the search box returns environments matching your search criteria.

{{< figure src="/images/automate/create-environment-button.png" alt="Create Environment Button">}}

#### Details of an Environment

Select a specific environment to view that environment's cookbook constraints and attributes.

##### Cookbook Constraints

 The selected bar contains the list of all the cookbook constraints with _Name_, their _Operator_, and _Version_ respectively. Select **Edit** to change the _Cookbook Constraints_.

  {{< figure src="/images/automate/cookbook-constraints-in-environment.png" alt="Cookbook Constraints In Environment">}}

##### Attributes

The attributes window shows all the default and overridden [attributes]({{< relref "attributes" >}}). Select **Expand All** or **Collapse All** to view or hide the attributes. Select **Edit** to change the _Default Attributes_ and _Override Attributes_.

{{< note >}} You can edit _Cookbook Constraints_ and the _Attributes_ only for environments with an existing _client_ for that _organization_.
{{< /note >}}

{{< figure src="/images/automate/attributes-in-environment.png" alt="Attributes In Environment">}}

#### Delete an Environment

The Chef Infra Server lets you delete environments one at a time. Select **Delete** from the more information (three dots) icon on the far right side of the environment that you want to delete, as shown below:

{{< figure src="/images/automate/delete-an-environment.png" alt="Delete an Environment by selecting the more information icon on the right side of an environment list item">}}

### Data Bags

[Data bags]({{< relref "data_bags" >}}) store global variables as JSON data. Data bags are indexed for searching and can be loaded by a cookbook or accessed during a search.

Chef Infra Server UI lets you:

- Create a data bag.
- List out all the data bags in one place.
- Search for a specific data bag item from a list of items.
- Create a data bag item.
- Edit a data bag item.
- Delete a data bag item.
- Delete a data bag.

#### Create a Data Bag

To create a new data bag, select **Create Data Bag**, as shown below:

{{< figure src="/images/automate/create-data-bag-button.png" alt="Create Data Bag Button">}}

Selecting the **Create Data Bag** button opens a dialog box. Enter the name and select **Create**.

{{< figure src="/images/automate/create-data-bag-popup.png" alt="Create Data Bag Dialog Box">}}

#### Create a Data Bag Item

To create a data bag item, select the data bag from the list of created data bags and follow the steps given below:

Select **Create Item**, as shown below:

{{< figure src="/images/automate/create-data-bag-item-button.png" alt="Create Data Bag Item Button">}}

Selecting the **Create Item** button opens a dialog box. Enter the **Data Bag Item ID** and the details of the key value pairs of that particular item. Select **Create**.

{{< figure src="/images/automate/create-data-bag-item-popup.png" alt="Create Data Bag Item Dialog Box">}}

#### Search for an Item

Use the **Search data bag items** bar to find an item from the list of data bag items. Entering the name of an item in the search bar returns data bag items that match your search criteria.

{{< figure src="/images/automate/create-data-bag-item-button.png" alt="Search Data Bag Item Bar">}}

#### Edit a Data Bag Item

Select a specific data bag item to view the details. The details contain an *id*, and a couple of _key values_. Chef Infra Server lets you **edit** a data bag item.

{{< figure src="/images/automate/edit-and-delete-data-bag-item.png" alt="Edit and Delete a Data Bag Item">}}

To edit the details of the data bag items, select **Edit**. In the dialog box, you can edit the details for the specific data bag item. Once done, Select **Save Item**.

{{< figure src="/images/automate/edit-data-bag-item.png" alt="Edit a Data Bag Item">}}

#### Delete a Data Bag Item

Select a specific data bag item to view the details. The details contain an *id*, and a couple of _key values_. Chef Infra Server lets you **delete** a data bag item.

{{< figure src="/images/automate/edit-and-delete-data-bag-item.png" alt="Edit and Delete a Data Bag Item">}}

To delete a data bag item, select **Delete**. In the dialog box displayed, select **Delete** to delete the data bag item.

{{< figure src="/images/automate/delete-data-bag-item.png" alt="Delete a Data Bag Item">}}

#### Delete a Data Bag

Chef Infra Server lets you delete the existing data bag one at a time. To delete a data bag, select the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-data-bag.png" alt="Delete a Data Bag">}}

### Clients

Chef Infra Clients provide secure API access to the Chef Infra Server. Chef Infra Server UI lets you:

- Create a client.
- Search for a specific client from a list of clients.
- List out all the clients at one place.
- Reset a client key.
- Delete a client.

#### Create a Client

To create a new client, select **Create Client**.

{{< figure src="/images/automate/create-client-button.png" alt="Create Client Button">}}

The **Create Client** button opens a dialog box. Enter the _Client Name_ and select _Validation Client_ to create a Validation Client. Select **Create**.

{{< figure src="/images/automate/create-client-popup.png" alt="Create Client Dialog Box">}}

Selecting **Create** opens a dialog box which contains the _Private Key_ of that particular client. Select **Download** to download the _Private Key_.

{{< figure src="/images/automate/client-private-key.png" alt="Create Private Key of a Client">}}

#### Search for a Client

Use the **Search Clients** bar to find an client from the list of clients.
Entering the name of a client in the search bar returns clients matching your search criteria.

{{< figure src="/images/automate/create-client-button.png" alt="Create Client Button">}}

#### Public Key of a Client

Select the client to view the _Public Key_ for that client. The _Public Key_ of that client will be displayed in **Details**.

{{< figure src="/images/automate/client-details-public-key.png" alt="Public Key of Clients">}}

The Chef Infra Server lets you reset the _Public Key_ using the _Reset Key_ option. Selecting the _Reset Key_ option opens an alert stating _The current key will no longer be accepted._

{{< figure src="/images/automate/reset-key-in-client-details.png" alt="Reset Key option of a Client" width="400" height="300">}}

Selecting the _Reset Key_ shown in the above image opens a new dialog box which contains the new _Private Key_ of that client. To download the new _Private Key_, select **Download**.

{{< figure src="/images/automate/reset-public-key-of-a-client.png" alt="Reset Public Key of a Clients">}}

#### Delete a Client

Chef Infra Server lets you delete the existing clients one at a time. To delete a client, select the ellipses icon and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-client.png" alt="Delete a Client">}}

## Troubleshoot

While fetching any objects like cookbooks, you might face an error `Could not get cookbooks: organization 'no-org' does not exist`, which means the provided organization does not exist on Chef Infra Server.

{{< figure src="/images/automate/could-not-get-cookbooks-organization.png" alt="Could not get cookbooks: Organization">}}

Create the organization using the knife command, `knife opc org create` or the Chef Infra Server CLI command, `chef-server-ctl org-create`, then add the _Name_, _Projects_, _Admin User_, and _Admin Key_ to fetch the objects.
