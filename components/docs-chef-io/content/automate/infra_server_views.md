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

## Roles

## Data Bags

## Environments

## Clients
