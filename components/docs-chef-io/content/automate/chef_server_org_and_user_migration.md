+++
title = "Chef Infra Server Org and User migration"
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Server Org and User migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef_server_org_and_user_migration.md Chef Infra Server Org and User Migration"
    weight = 30
+++

## Overview

The _Chef Infra Server Org and User Migration_ page (**Infrastructure > Chef Servers > Chef server details**) lets you connect existing Chef Infra Server organisations and users to Chef Automate, migrate all the organisations and user into Automate using the simple migration process.


## Chef Server Org and User Sync steps  

###Prerequisite

- Chef Server backup using `knife ec backup` command is available
- Chef Server is added to the Infra Server View

When a server is added to Automate, organisations and users are not associated with Automate.
Automate does not list any users or organisations of the server.  

{{< figure src="/images/automate/chef_server_details_page.png" width="500" alt="Chef Server Details Page">}}

Ths process syncs all the organisations and users of a server into Automate.  

**Note:**  
The sync of organisation and users is only available to Automate admins or users with owner privilege in Automate.

###Uploading Chef Server backup  
To migrate existing organisations and users to the Chef Automate infrastructure, click the `sync org and users` button inside Chef server details box.

When it is clicked, a slider opens up with the option to upload the zip file.

{{< figure src="/images/automate/sync_org_and_user_slider.png" width="500" alt="Chef Server Details Page">}}

Upload a zip file (either select the zip file or drag and drop the zip file) after selecting a zip file, click on `upload file` button.

{{< figure src="/images/automate/select_zip_file_page.png" width="500" alt="Chef Server Details Page">}}

###Starting Sync
Once click on the `upload file` button, zip file is uploaded and sync process starts.

You can view the progress of the sync on the Server details section in the Server details page.
The sync process has 12 steps in total. The progress tells you the number of the steps completed.

{{< figure src="/images/automate/migration_is_in_progress_page.png" width="500" alt="Chef Server Details Page">}}

After parsing the zip file content is completed, a preview link is shown beside the progress label terming `Sync in Progress`.  

**Need to add the screenshot**

On clicking the `Click to Preview` link, the preview screen is shown.

**Need to add the screenshot**

###Canceling Sync
At this point you can cancel the sync by clicking on the cancel button.
Cancelling the sync will not have any modification to the system.

###Previewing Sync
The preview screen gives the following information related to organisations where you can view
- The number of total organisations to be updated
- The number of organisations already available with Automate and needs no modification or is skipped
- The number of organisations to be deleted

The preview screen also lists down the users to be added to Automate.
Please refer to this **link to user permission** to learn about user permission sync.

**Need to add the screenshot**

The preview screen indicates users which are available in Automate system associated with a different Chef Server.  
In case of such cases, Automate does not take any decisive action but gives a provision to the admin to provide a new username.
These users are highlighted and a text box is enabled to input the corresponding new username.

**Need to add the screenshot** 

###Confirming Sync
The syc continues on clicking the `Confirm` button.
On completion of the sync, the `Complete` status is displayed

###Failed Sync:

In sync process, if something goes wrong and sync is not completed, in that case sync is failed. 
The status will show under the metadata section as `Failed`.   
The cause of failure can be viewed on hovering the error icon.

**need to add the screenshot**
 
###Outcome of the sync
- All the organisations are synced to Automate and linked to the Infra Server.
- Every organisation has an IAM project associated
- All the users from the Chef Server are synced.
- The users are added to the Chef Server and associated with the organisation and the corresponding IAM projects.
- The users are added to the IAM policies of the IAM projects based on the permission user holds in Chef Server.
  Please refer to this **link to user permission** to learn about user permission sync.

##User permission Sync
Automate on sync creates a unique project for every new organisation added.  
The different policies and their associated permissions are as below:

|                | Cookbooks | Roles                      | Environments               | Attributes                 | Databags                   | Clients                    | Nodes                      | PolicyFiles                | Policy Group               | Users under an Org | Add Infra Server | 
|----------------|-----------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|--------------------|------------------|
| Viewer         | Read      | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | No access          | No access        | 
| Editor         | Read      | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | No access          | No access        | 
| Project Owner  | Read      | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read               | No access        |

The users under admin group in an organisation are added to Project Owner policy.
The users under any group other than admin in an organisation are added to Editor policy.