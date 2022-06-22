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
    weight = 30
+++

## Overview

The _Chef Infra Server_ page (**Infrastructure > Chef Servers**) lets you connect existing Chef Infra Servers to Chef Automate, view all the connected Chef Infra Servers, and manage all of the objects on your connected Chef Infra Servers.

The _Chef Infra Server_ acts as a hub for configuration data. The Chef Infra Server stores _cookbooks_, the policies that are applied to _nodes_, and metadata that describes each registered node managed by Chef Infra Client.

Nodes use Chef Infra Client to ask the Chef Infra Server for configuration details, such as recipes, templates, and file distributions. Chef Infra Client then does as much configuration work as possible on the nodes themselves (and not on the Chef Infra Server).

This scalable approach distributes the configuration effort throughout the organization.

{{< figure src="/images/automate/chef-server-page.png" alt="Chef Infra Server Page">}}

The objects that you can manage from the Chef Infra Server are:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients
- Nodes
- Policyfiles
- PolicyGroup

## Connect Chef Infra Servers to Chef Automate

The _Chef Infra Server_ panel starts with an empty list of servers.

To add existing Chef Infra Servers to the Chef Automate infrastructure, select **Add Chef Server**, which will request the name, FQDN, and IP address of your Chef Infra Server:

{{< figure src="/images/automate/add-chef-server-popup-menu.png" width="500" alt="Add Chef Server Form">}}

Chef Automate warns you if you enter an invalid FQDN or IP address:

{{< figure src="/images/automate/add-chef-server-popup-menu-with-error.png" width="500" alt="Add Chef Server Form">}}

Once done, select **Add Chef Server** and see the server in the list of Chef Infra Servers.

## Connect a Chef Organization to a Chef Infra Server

To view a list of existing [organizations]({{< relref "server_orgs" >}}) on a Chef Infra Server,
select a Chef Infra Server from the **Chef Servers** list, then select the **Organizations** tab, as shown below:

{{< figure src="/images/automate/chef-server-organization.png" alt="Chef Infra Server Organization">}}

To create a new organization, see the documentation on [setting up a Chef Infra Server]({{< relref "infra_server#set-up-the-chef-infra-server" >}}).

To add an existing organization, select **Add Chef Organization**, which opens a dialog box as shown below:

{{< figure src="/images/automate/add-chef-organization-popup-menu.png" width="500" alt="Add Chef Organization Form">}}

Enter the _Name_, _Projects_, _Admin User_, and _Admin Key_ fields using the same values provided when the organization gets configured using _Knife_.
Copy the contents of the `~/.chef/USER.pem` file and paste it into the **Admin Key** field. Then select **Add Chef Organization** to add the organization to the Chef Infra Server.

## Access Chef Infra Server Components

Manage the following Chef Infra Server components for an organization using Chef Automate:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

To access these components for an organization, select **Chef Servers > Organizations** and then select an organization. Refer to the image below:

{{< figure src="/images/automate/chef-infra-server-objects.png" alt="Chef Infra Server Objects">}}

### Cookbooks

A [cookbook]({{< relref "cookbooks" >}}) is the fundamental unit of configuration and policy distribution. A cookbook contains recipes and other files, components, or directories.

The Chef Infra Server lets you view:

- List of cookbooks with their latest version
- Different versions of a cookbook
- Contents of a cookbook
- Details of a cookbook

#### List of Cookbooks

In Chef Infra Server, you can view all the cookbooks of an organization. The latest version of the cookbooks is mentioned in the list.

{{< figure src="/images/automate/list-of-cookbooks.png" alt="List of Cookbooks">}}

Select a cookbook from the list to view the details of that cookbook.

#### Different Versions of Cookbooks

To view different versions of a cookbook:

- Select a cookbook from the list.
- View the different versions of the cookbook using the dropdown list.

{{< figure src="/images/automate/view-different-versions-of-cookbook.png" alt="Versions of a Cookbook">}}

#### Cookbook Content

Select the **Content** tab to view the recipes and other components of a cookbook. This view also contains the files or directories of the components. To view the cookbook content, select the file from the component list.

{{< figure src="/images/automate/contents-of-a-cookbook.png" alt="Cookbook Contents">}}

#### Details of a Cookbook

Select the **Details** tab to view the requirements, usage, resources, and license.

{{< figure src="/images/automate/details-of-a-cookbook.png" alt="Details of a Cookbook">}}

### Roles

[Roles]({{< relref "roles" >}}) let you define patterns and processes that exist across nodes in an organization as belonging to a single job function. Each role consists of zero (or more) attributes and a run-list.

The Chef Infra Server lets you:

- Create a role
- View all roles
- Search for a specific role
- View the details of roles:
  - Run List
  - Attributes
- Delete a role

#### Create a Role

To create a new role, select **Create Role**:

{{< figure src="/images/automate/create-role-button.png" alt="Create Roles Button">}}

Selecting the **Create Role** button opens a dialog box. The dialog box contains four sections: _Details_, _Run List_, _Default Attributes_, and _Override Attributes_. Enter the details in the possible sections and select **Create** to create a new role.

{{< figure src="/images/automate/create-role-popup.png" alt="Create Roles Dialog Box">}}

#### Search for a Role

Use the **Search Roles** bar to find the existing role from the list of roles. Entering the name of a role in the search box returns roles matching your search criteria.

{{< figure src="/images/automate/create-role-button.png" alt="Select Create Roles Button">}}

#### View Role Details

Select a specific role to view the details of that particular role. It contains two tabs, _Details_ and _Attributes_.

{{< figure src="/images/automate/view-role-details.png" alt="View Role Details">}}

The _Details_ contains the Run List, whereas the attributes tab displays the _default_ and the _override_ attributes of the role.

{{< figure src="/images/automate/view-attributes-details.png" alt="View Attributes Details">}}

You can edit the _default_ and _override_ attributes by selecting the **Edit** option. It opens a pop-up window where you can make the changes and select **Save**.

#### Delete a Role

Chef Infra Server lets you delete the existing roles one at a time. To delete a role, select the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-role.png" alt="Delete A Role">}}

### Environments

An [environment]({{< relref "environments" >}}) can be used to map an organization's real-life workflow to what can be configured and managed when using Chef Infra. This mapping is accomplished by setting attributes and pinning cookbooks at the environment level.

The Chef Infra Server UI lets you:

- Create an environment
- View all environments
- Search for a specific environment
- View the details of an environment
- Edit an environment
- Delete an environment

#### Create an Environment

To create a new Environment, select **Create Environment**, as shown below:

{{< figure src="/images/automate/create-environment-button.png" alt="Create Environment Button">}}

Selecting the **Create Environment** button opens a dialog box. The dialog box contains four sections: _Details_, _Constraints_, _Default Attributes_, and _Override Attributes_. Enter the details in the possible sections, and select **Create** to create a new environment.

{{< figure src="/images/automate/create-environment-popup.png" alt="Create Environment Dialog Box">}}

#### Search for an Environment

Use the **Search environments** bar to find an existing environment from the list.
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

- Create a data bag
- View all data bags
- Search for a specific data bag item
- Create a data bag item
- Edit a data bag item
- Delete a data bag item
- Delete a data bag

#### Create a Data Bag

To create a new data bag, select **Create Data Bag**, as shown below:

{{< figure src="/images/automate/create-data-bag-button.png" alt="Create Data Bag Button">}}

Selecting the **Create Data Bag** button opens a dialog box. Enter the name and select **Create**.

{{< figure src="/images/automate/create-data-bag-popup.png" alt="Create Data Bag Dialog Box">}}

#### Create a Data Bag Item

To create a data bag item, select the data bag from the list of data bags and follow the steps given below:

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

To edit the details of the data bag items, select **Edit**. In the dialog box, you can edit the details for the specific data bag item. Once done, select **Save Item**.

{{< figure src="/images/automate/edit-data-bag-item.png" alt="Edit a Data Bag Item">}}

#### Delete a Data Bag Item

Select a specific data bag item to view the details. The details contain an _id_ and a couple of _key values_. Chef Infra Server lets you **delete** a data bag item.

{{< figure src="/images/automate/edit-and-delete-data-bag-item.png" alt="Edit and Delete a Data Bag Item">}}

To delete a data bag item, select **Delete**. In the dialog box displayed, select **Delete** to delete the data bag item.

{{< figure src="/images/automate/delete-data-bag-item.png" alt="Delete a Data Bag Item">}}

#### Delete a Data Bag

Chef Infra Server lets you delete the existing data bag one at a time. To delete a data bag, select the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-data-bag.png" alt="Delete a Data Bag">}}

### Clients

Chef Infra Clients provide secure API access to the Chef Infra Server. Chef Infra Server UI lets you:

- Create a client
- View all clients
- Search for a specific client
- Reset a client key
- Delete a client

#### Create a Client

To create a new client, select **Create Client**.

{{< figure src="/images/automate/create-client-button.png" alt="Create Client Button">}}

The **Create Client** button opens a dialog box. Enter the _Client Name_ and select _Validation Client_ to create a Validation Client. Select **Create**.

{{< figure src="/images/automate/create-client-popup.png" alt="Create Client Dialog Box">}}

Selecting **Create** opens a dialog box that contains the _Private Key_ of that particular client. Select **Download** to download the _Private Key_.

{{< figure src="/images/automate/client-private-key.png" alt="Create Private Key of a Client">}}

#### Search for a Client

Use the **Search Clients** bar to find a client from the list of clients.
Entering the name of a client in the search bar returns clients matching your search criteria.

{{< figure src="/images/automate/create-client-button.png" alt="Create Client Button">}}

#### Public Key of a Client

Select the client to view the _Public Key_ for that client. The _Public Key_ of that client will be displayed in **Details**.

{{< figure src="/images/automate/client-details-public-key.png" alt="Public Key of Clients">}}

The Chef Infra Server lets you reset the _Public Key_ using the _Reset Key_ option. Selecting the _Reset Key_ option opens an alert stating _The current key will no longer be accepted._

{{< figure src="/images/automate/reset-key-in-client-details.png" alt="Reset Key Option of a Client" width="400" height="300">}}

Selecting the _Reset Key_ shown in the above image opens a new dialog box containing the client's new _Private Key_. To download the new _Private Key_, select **Download**.

{{< figure src="/images/automate/reset-public-key-of-a-client.png" alt="Reset Public Key of a Clients">}}

#### Delete a Client

Chef Infra Server lets you delete the existing clients one at a time. To delete a client, select the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**, as illustrated below:

{{< figure src="/images/automate/delete-a-client.png" alt="Delete a Client">}}

### Nodes

A [node]({{< relref "/nodes" >}}) is a device that is managed by Chef Infra. During the Chef Infra Client run, the Infra Client retrieves [attributes](/nodes#attributes) that defines the expected state of the node and a [run-list](/nodes#run-lists) that defines how a node can be configured to that state from the Infra Server. The Infra Client then uses that information to update the node to its expected state.

The Chef Infra Server integration lets you:

- Search for a specific node
- View all nodes
- Details of a node
- Edit Run list
- Edit Attributes
- Manage tags
- Reset a node key
- Delete a node

#### Search for a Node

Use the search bar on the Nodes tab (**Chef Infra Servers > Nodes**)  to find a node from the list of nodes. Entering the name of a node in the search bar returns nodes matching your search criteria.

{{< figure src="/images/automate/search-a-node.png" alt="Search a Node">}}

#### Details of a Node

Select a specific node to view the node information, metadata, and details of the node's environment. The first section of the page has the **Node Information** like `Environment`, `Policy Group`, and `Policy Name`. You can also view the **Metadata** for the node that contains the name of the `Chef Server`, and the name of the `Chef Organization`.

The above information looks like as shown below:

{{< figure src="/images/automate/node-information-metadata.png" alt="Node Information & Metadata">}}

The next section of the page contains two options:

- Details
- Run List
- Attributes

##### Details

The **Details** section lets you view and update the node environment. The drop-down menu consists of the list of environments created in the chef server. You can select any one environment for the selected node.

To select the environment:

- Select the dropdown menu and the name of the environment.

- You can add tags to the environment from the text bar below the dropdown menu.
**Note:** To add multiple tags at a time, use a comma separator. Example: Tag1, Tag2, Tag3.

- Selecting an environment opens a pop-up to save the node environment. Select **Save** to change the node environment.

{{< figure src="/images/automate/update-node-environment.png" alt="Update Node Environment">}}

##### Run List

You can edit, expand or collapse a node's run list.

{{< figure src="/images/automate/run-list-of-a-node.png" alt="Run List of a Node">}}

Edit a run list by:

1. Select **edit**. A pop-up window opens.

2. The left side of the window contains the list of environments. Select an environment from the list.

3. The right side of the window shows the run list for the selected environment.

4. Select the run list for a specific environment.

5. Select **Create**.

This opens a pop-up window similar to:

{{< figure src="/images/automate/edit-run-list.png" alt="Edit Run List">}}

You can find the editing window directly by selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Edit Run List**  of a specific node in the node list. Selecting the option opens a pop-up for editing the run list.

##### Attributes

The attributes window shows all the default and overridden [attributes]({{< relref "attributes" >}}). Select **Expand All** or **Collapse All** to view or hide the attributes. Select **Edit** to change or update the existing attributes.

{{< figure src="/images/automate/node-attributes-list.png" alt="Node Attributes List">}}

You can find the editing window directly by selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Edit Attributes**  of a specific node in the node list. Selecting the option opens a pop-up for editing the attributes.

#### Manage Tags

Chef Infra allows you to manage tags of the environment. You can add or remove multiple tags in two ways:

1. Selecting the node and add the tags from the text bar below the dropdown menu of environments.

2. Select the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Manage Tags** for a specific node. Add and remove tags in tag editor and select **Update Tags** to save your changes.

{{< figure src="/images/automate/manage-tags-from-ellipses-icon.png" alt="Manage Tags">}}

#### Reset a Client Key

Reset a client key by selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Reset Key** of a specific node in the node list. Selecting **Reset Key** opens a warning _The current key will no longer be accepted_. Select **Reset Key** once again to confirm.

{{< figure src="/images/automate/reset-the-node-key.png" alt="Reset the Client Key">}}

#### Delete a Node

Delete individual existing nodes by selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**:

{{< figure src="/images/automate/delete-a-node.png" alt="Delete a Node">}}

### Policyfiles

[Policyfiles]({{< relref "/policyfile" >}}) are preferred way of managing roles, environments, and community cookbooks data with a single document that is uploaded to the Chef Infra Server. Policyfiles lets you test and promote codes with a simpler interface.

The Chef Infra Server integration lets you:

- Search for specific Policyfiles
- View all the Policyfiles
- View the details of Policyfiles:
  - Content
  - Attributes
  - Revision ID
- Delete Policyfiles

#### Search for Policyfiles

Use the search bar on the Policyfile tab (**Chef Infra Server > Policyfile**) to find a policy file from the list. Enter the name of a policy file in the search bar to view matching Policyfiles.

{{< figure src="/images/automate/policyfiles-list-and-searchbar.png" alt="Policyfile">}}

#### Details of Policyfiles

Select a Policyfile to view the **METADATA**, **Content**, **Attributes**, **Revision ID**, and **Cookbook Dependencies** of that Policyfile.

{{< figure src="/images/automate/details-of-policyfiles.png" alt="Details of Policyfiles">}}

##### Content Tab

The **Content** tab contains the list of all:

- Included Policies
- Run Lists

###### Included Policies

Select a specific Policyfile from the _Included Policies_ to view the details.

{{< figure src="/images/automate/content-scroller-tab-for-policyfiles.png" alt="Content Scroller in Policyfiles Details">}}

In the above image, the slider tab shows the _Details_ of the selected policyfile. To view the _Revision ID_ select the **Revisions** option in the tab.

###### Run List

Select a specific Run Item from the Run List to view the details in a slider tab.

{{< figure src="/images/automate/runlist-tab-for-policyfiles.png" alt="Runlist Tab under Policyfiles">}}

To view the cookbook details of the item, select the **Go to Cookbook Details** option in the tab.

##### Attributes Tab

The **Attributes** tab displays the _default_ and the _override_ attributes of a Policyfile.

{{< figure src="/images/automate/attributes-tab-of-policyfiles.png" alt="Attributes tab of Policyfiles">}}

You can **Expand** and **Collapse** the *default* and *override* attributes by selecting the option.

##### Cookbook Dependencies

Select the **Cookbook Dependencies** button to view all the _Dependencies Rules_ and _Cookbook_ of the policyfile in table format. Select a specific cookbook from the list to view the details of the cookbook.

{{< note >}} Selecting a specific cookbook redirects you to the audit section of the **Cookbook** main tab.
{{< /note >}}

The slider tab contains the `Operator` and `Version` of the cookbooks.

{{< figure src="/images/automate/cookbook-dependencies-from-policyfiles.png" alt="Cookbook Dependencies Button">}}

{{< note >}}
You can find the **Revision ID** window directly by selecting the ellipses icon of a Policyfile. Selecting the option opens a pop-up that contains the detailed revision ID.
{{< /note >}}

{{< figure src="/images/automate/revision-id-of-policyfiles.png" alt="Revision id of Policyfiles">}}

#### Delete Policyfiles

Delete individual Policyfiles by selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}} and then **Delete**:

{{< figure src="/images/automate/delete-policyfiles.png" alt="Delete Policyfiles">}}

### Policy Group

The Chef Infra Server integration lets you:

- Search for a Policy Group
- View all the Policy Groups
- View the details of a Policy Group
  - Policyfiles
  - Nodes

#### Search for a Policy Group

The search bar on the Policy Groups tab (**Chef Infra Server > Policy Groups**) finds a policy group from the list. Enter the name of a policy group in the search bar to view matching Policy Groups.

{{< figure src="/images/automate/search-for-a-policygroup.png" alt="Search for a Policy Group">}}

#### View the details of a Policy Group

Select a Policy Group to view the Policy Group Information and METADATA of that Policy Group.

{{< figure src="/images/automate/details-of-the-policygroup.png" alt="Details of a Policy Group">}}

##### Policyfiles Tab

The details page of the policy group view the list of **Policyfiles**.

{{< figure src="/images/automate/policyfiles-under-policygroup.png" alt="Policyfiles under Policy Group">}}

Select a Policyfile from the list to view its details.

{{< note >}} Selecting a specific policyfile redirects you to the details section of the **Policyfiles** main tab.
{{< /note >}}

##### Nodes Tab

The details page of the policy group lets you view the list of **Nodes**.

{{< figure src="/images/automate/nodes-tab-in-policygroup.png" alt="Nodes Tab in Policy Group">}}

Select a node from the list to view its details

{{< note >}} Selecting a specific node opens details section of the **Nodes** tab.
{{< /note >}}

## Troubleshoot

While fetching any objects like cookbooks, you might face an error `Could not get cookbooks: organization 'no-org' does not exist`, which means the provided organization does not exist on Chef Infra Server.

{{< figure src="/images/automate/could-not-get-cookbooks-organization.png" alt="Could not get cookbooks: Organization">}}

Create the organization using the knife command, `knife opc org create` or the Chef Infra Server CLI command, `chef-server-ctl org-create`, then add the _Name_, _Projects_, _Admin User_, and _Admin Key_ to fetch the objects.
