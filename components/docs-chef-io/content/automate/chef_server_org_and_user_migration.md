+++
title = "Chef Infra Server Organization and User Migration"
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "User Migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef_server_org_and_user_migration.md Chef Infra Server Organization and User Migration"
    weight = 50
+++

This section lists the steps to connect the existing Chef Infra Server organization and users to Chef Automate and migrate all the organization and users into Chef Automate.

## Prerequisite

- Chef Server backup using `knife ec backup` command.
- Chef Server is added to the Infra Server View.

## Chef Server Organization and User Sync

When you add a server to Automate, the **organization** and **user** are not associated to the Automate as Automate never lists any **user** or **organization** to the server.

The image below refer to the details page of a Chef Server:

{{< figure src="/images/automate/chef_server_details_page.png" width="500" alt="Chef Server Details Page">}}

{{< note >}} The sync of organization and users will only be available to you if you are an Automate admin or a user with owner Privilege in Automate. {{< /note >}}

## Uploading Chef Server Backup

To migrate the existing organizations and users to the Chef Automate Infrastructure:

* Select **Sync Org and Users** button inside Chef Server detail box.

* A slider opens with an option to upload a file.

{{< figure src="/images/automate/sync_org_and_user_slider.png" width="500" alt="Chef Server Details Page">}}

* Upload the zip file and select **Upload File**.

{{< figure src="/images/automate/select_zip_file_page.png" width="500" alt="Chef Server Details Page">}}

## Start Sync

Once you have uploaded the file, the sync process starts. You can view the progress of the sync on the Server details section in the **Server Section** details page.

{{< figure src="/images/automate/migration_is_in_progress_page.png" width="500" alt="Chef Server Details Page">}}

Once the parsing of the `.zip` file content is completed, a preview link is shown beside the progress label terming **Sync in Progress**.  

**Need to add the screenshot**

Select the **Click to Preview** link to view the screen.

**Need to add the screenshot**

## Cancel Sync

To cancel the sync, select **Cancel** button. The cancellation will not modify anything in the system.

**Need to add the screenshot**

## Previewing Sync

The preview screen gives the information related to organizations where you can view:

- The number of total organizations to be updated.
- The number of organizations already available with Automate and need no modification or is skipped.
- The number of organizations to be deleted.

You can also view the list of users to be added to Automate on preview screen. Refer to the **Link to User Permission** to learn about User Permission Sync.

**Need to add the screenshot**

The preview screen helps in letting you know which are available in Automate system associated with different Chef Server. Automate does not take any decisive action but gives a provision to the admin to provide a new **username**.
These users are highlighted and a text box is enabled to add the corresponding new **username**.

**Need to add the screenshot** 

## Confirming Sync

The sync will continue if you select the **Confirm** button. The Automate will show the **Compete** status when are sync is completed.

**Need to add the screenshot** 

## Failed Sync

The sync might fail and show the **Failed** status under metadata. You can get to know about the reason of failure bu hovering the **Error** icon.

**Need to add the screenshot**

## Output of Sync

* All the organizations are synced to Automate and linked to Chef Infra Server.
* Every organization has an **IAM** project associated.
* All the users from the Chef Server are synced.
* The users are added to the Chef Server and associated with the organixation and the corresponding **IAM** projects.
* The users are added to the **IAM** policies of the IAM projects based on the permission user holds in Chef Server. Refer to the **Link to User Permission** to learn more about User Permission Sync.

## User Permission Sync

Automate on sync creates a unique project for every newly added organization. The different policies and their associated permissions are listed below:

|                | Cookbooks | Roles                      | Environments               | Attributes                 | Databags                   | Clients                    | Nodes                      | PolicyFiles                | Policy Group               | Users under an Org | Add Infra Server | 
|----------------|-----------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|--------------------|------------------|
| Viewer         | Read      | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | Read                       | No access          | No access        | 
| Editor         | Read      | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | No access          | No access        | 
| Project Owner  | Read      | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read               | No access        |

{{< note >}} The users under admin group in an organization are added to **Project Owner Policy**. {{< /note >}}

{{< note >}} The users under any group other than admin in an organization are added to **Editor Policy**. {{< /note >}}
