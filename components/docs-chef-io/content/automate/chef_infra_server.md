+++
title = "Client Runs"

date = 2021-03-12T12:01:58-07:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Server"
    parent = "automate"
    identifier = "automate/chef_infra_server.md Chef Infra Server"
    weight = 60
+++

## Overview

The __Chef Server__ page shows all the Chef Infra Servers connected to Chef Automate. The __Chef Server__ user interface can be used to view and manage all the Chef Infra Server Objects like:

- Cookbooks
- Roles
- Environments
- Data Bags
- Clients

## Connect to the Chef Infra Server

To start making use out of the Chef Infra Server, firstly deploy and run the [Chef Automate](https://docs.chef.io/automate/install/#installation-guide).

**Note:** No explicit configuration is required to install the Automate Infra views.

Login to Chef Automate using the credentials. Once logged in, the page looks like as shown below:

![Chef Automate](/images/automate/chef-automate-on-chef-infra-page.png)

Click on **Infrastructure** located at the top bar of the page. Refer to the image below:

![Chef Automate with Highlighted Infrastructure](/images/automate/chef-automate-infrastructure.png)

Click on the **Chef Servers** located at the left panel of the page. The page looks like as shown below:

![Chef Infra Server](/images/automate/chef-server-page.png)

The above image is the **Chef Server** page which contains all the Chef Infra Servers connected to the Chef Automate. The Chef Infra Servers listed are the ones which are already configured and added to the Chef Automate. To create a new Infra Server click [here](https://docs.chef.io/automate/infra_server/).

To add the created Chef Server to the Infrastructure, click on **Add Chef Server** button. Refer to the image below:

![Add Chef Server](/images/automate/add-chef-server-button.png)

Clicking on the *Add Chef Server** button opens a popup menu as shown below:

![Add Chef Server Popup Menu](/images/automate/add-chef-server-popup-menu.png)

Mention the correct Name, FQDN and the IP Address. Click on **Add Chef Server**. The desired server gets added to the Chef Infra Server 

**Note:** If the mentioned FQDN and IP Address is incorrect, the box displays and error as shown below:

![Add Chef Server Popup Menu](/images/automate/add-chef-server-popup-menu-with-error.png)

Click on the listed Servers to view the details of the specific **Chef Server**.

## Connect to the Chef Infra Server Organization

Selecting a specific added Chef Server opens the list of organizations in the Server. Refer to the Image below:

![Chef Infra Server Organization](/images/automate/chef-server-organization.png)

The Organization in the Chef server listed are the ones which are already created in that specific server. To create a new Organization in tmhe Server click [here](https://docs.chef.io/automate/infra_server/#set-up-the-chef-infra-server).



