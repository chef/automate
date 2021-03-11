+++
title = "Chef Infra Server Views"
date = 2021-02-24T13:19:02-07:00
draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Infra Server Views"
    parent = "automate"
    identifier = "automate/infra_server_views.md Chef Infra Server Views"
    weight = 60
+++

## Overview

The _Chef Servers_ page shows all Chef Infra Servers connected to Chef Automate. To views & manage all the Chef Infra Server Objects ie. Cookbooks, Roles, Data bags, Environments and Clients.

Mutiple Chef Infra can be connected with Automate Infra Views.

## Prerequisites

- Deploy and run Chef Automate if not already https://docs.chef.io/automate/install/#installation-guide as Automate Infra views are by getting added by default, not explicit configuration required to install it.
- Deploy the Chef Infra Server if not already https://docs.chef.io/automate/infra_server/#install-a-standalone-chef-infra-server- Configure and create Chef Organization, Chef Infra Server User credentials.
- Setup knife(optional) with provided Chef Infra Server details https://docs.chef.io/workstation/knife_setup/
Note: Exactly same details required to connect with Automate Infra views.

```
# Example .chef/credentials file to connect with knife
[default]
node_name = "barney"   # knows as client_name or user_name
client_key = "barney_rubble.pem"   # client key
chef_server_url = "https://CHEF_SERVER_FQDN_OR_IP_ADDRESS/organizations/ORGANAZATION_NAME"
```

- Same short of detail required to connect with Automate infra views and can be filled details using `Add Chef Server` and `Add Chef Oraganization`


## Connect to Chef Infra Server

![Connect to Chef Infra Server](/images/automate/add-chef-server.png)

## Connect to Chef Infra Server Organization

![Connect to Chef Infra Server Organization](/images/automate/add-chef-organization.png)


## Access Chef Infra Server Objects

In Automate Infra views following are objects can be manage and accessed:
 - Cookbooks
 - Roles
 - Data Bags
 - Environments
 - Clients

What are features not covered in Automate Infra Views:
- Manage permissons
- User Invitation
- User Signup
- Login window for Chef Infra Server Users.

Features lineup:
 - Full control on Roles, Data Bags, Environments and clients objects.
 - Full control on Nodes ie. Edit Attributes, Edit recipes, Edit environment, Manage Tags etc.
 - Policyfiles views. 
 - Manage Global objects like Organizations, Groups, Users and Starter Kit.

## Cookbooks
- List: List out all the cookbooks available in the system, with the name and the current version of the cookbook.
- Cookbook detail: Fetching the detail of current cookbooks and dropdown to select a specific version of the cookbook.
  - Content: View all files with folder structure and showing the content of the first file. Clicking on the left sidebar on a specific file could show the content of that file.
  - Details: Showing the detail about the cookbook.

## Roles
- List: List out all the roles available in the system, with name, description, associated environments. Ability to search based on the role name.
  - List Action: Ability to Delete the role.
- Role Detail: Clicking on list role item, navigate to role detail.
  - Run List: Expanded run list with name, version, and type.
  - Attributes: It showed the default and override attributes with Expand All and Collapse All actions.

## Environments
- List: List out all the environments available in the system, with name description. Ability to search the environment based on the name
  - List Action: Ability to Delete the environment.
- Environment Detail: Clicking on the list environment item, navigate to the environment detail page.
  - Cookbook Constraints: List out all the cookbook constraints with name, operator, and version.
  - Attributes: It showed the default and override attributes with Expand All and Collapse All actions.

## Data Bags
- List: List out all the data bags available in the system, with the name.
  - List Action: Ability to Delete the data bag.
- Data Bag Detail: Clicking on list data bag item, navigated to detail and list out available items for the data bag.
- Data Bag Items List: Ability to search based on data bag item id, clicking on specific item showing the detail about the item.
- Data Bag Item Detail: It will show all data consist of the data bag item.

## Clients
- List: List out all the clients available in the system, with the name.
  - List Action: Ability to Delete the client.
  - Create Client: Ability to create a client.
- Client Detail: Clicking on a list item, navigated to client detail. it showing the public key with the Reset Key option.
