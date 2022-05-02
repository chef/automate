+++
title = "Chef Infra Sever Migration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Infra Server Migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef-infra-server-migration.md Chef Infra Server Migration"
    weight = 40
+++

Chef Infra Server has two global entities, **Users** and **Organizations**. Chef Infra Server Migration ensures that both Users and Organizations migrate to Chef Automate. This section lists the steps to connect the existing Chef Infra Server Users and Organizations to Chef Automate and migrate all the Users and Organizations into Chef Automate.

## Prerequisites

- Chef Server backup using `knife ec backup` command.
- Chef Server is added to the Infra Server View.

## Knife EC Backup

*knife-ec-backup* is the command to take backup of Chef Infra Server/Backend during Chef Infra Server Migration. *knife-ec-backup* can backup and restore the data in a Chef Infra Server installation and preserves the data in an intermediate and editable text mode. It is similar to the `knife download` and `knife upload` commands. It uses the same underlying libraries and includes workarounds for objects not yet supported by those tools and various Infra Server API deficiencies.

### Prerequisites

- Chef Infra Server 11.8 and above.

### Installation

#### Chef Infra Server (Recommended)

Install this gem with Chef Infra Server 12 and later, and the sub-commands are available with an embedded copy of `knife`. Refer to the command shown below:

```cmd
sudo /opt/opscode/bin/knife ec backup ~/chef-server-backup-directory
```

If you need a newer version of `knife-ec-backup` you can install it using the embedded `gem` command as shown below:

```cmd
/opt/opscode/embedded/bin/gem install knife-ec-backup --no-doc
```

### Run the `knife-ec-backup` Command

Run the `knife-ec-backup` command when logged in to your Chef Server machine.

For example: `/opt/opscode/bin/knife ec backup --server-url https://ec2-18-xxx-112-xxx.us-east-2.compute.amazonaws.com ./backup/ --with-user-sql --with-key-sql -c /etc/opscode/pivotal.rb`.

Refer to some supported commands below:

- `--sql-host`: The hostname of the Chef Infra Server's PostgreSQL Server. (default: `localhost`).

- `--sql-port`: The PostgreSQL listening port on the Chef Infra Server. (default: `5432`).

- `--sql-db`: The PostgreSQL Chef Infra Server database name. (default: `opscode_chef`) Specify `automate-cs-oc-erchef` when using Automate Chef Infra Server API.

- `--sql-user`: The username of PostgreSQL user with access to the `opscode_chef` database. (default: `autoconfigured` from */etc/opscode/chef-server-running.json*)

- `--sql-password`: The password for the `sql-user`. (default: `autoconfigured` from */etc/opscode/chef-server-running.json*)

### Output of the Command

The command takes the backup of the CS in the allotted folder. The folder structure is: `drwxr-xr-x 5 root root 4096 Dec 28 14:26`.

- drwxr-xr-x 6 ubuntu ubuntu 4096 Dec 28 15:54 ..
- -rw-r--r-- 1 root root 10642 Dec 28 14:26 key_dump.json
- -rw-r--r-- 1 root root 19423 Dec 28 14:26 key_table_dump.json
- drwxr-xr-x 10 root root 4096 Dec 28 14:26 organizations
- drwxr-xr-x 2 root root 4096 Dec 28 14:26 user_acls
- drwxr-xr-x 2 root root 4096 Dec 28 14:26 users

### Zip and Download to Local

You can zip and download the *knife-ec-backup* command to local by running the following command:

```cmd
zip -r directory.zip directory
```

{{< note >}} The above command is one of the options to zip the *knife-ec-backup* command. You can also run the command using any other option. {{< /note >}}

## Chef Server Organization and User Sync

When you add a server to Automate, the **organization** and **user** are not associated with the Automate as Automate never lists any **user** or **organization** to the server.

The image below refers to the details page of a Chef Server:

{{< figure src="/images/automate/chef_server_details_page.png" alt="Chef Server Details Page">}}

{{< note >}} The sync of organization and users will only be available to you if you are an Automate admin or a user with owner Privilege in Automate. {{< /note >}}

## Uploading Chef Server Backup

To migrate the existing organizations and users to the Chef Automate Infrastructure:

- Select the **Sync Org and Users** button inside the Chef Server detail box.

- A slider opens with an option to upload a file.

{{< figure src="/images/automate/sync_org_and_user_slider.png" alt="Chef Server Details Page">}}

- Upload the zip file and select **Upload File**.

{{< figure src="/images/automate/select_zip_file_page.png" alt="Chef Server Details Page">}}

## Start Sync

Once you have uploaded the file, the sync process starts. You can view the sync progress the Server details section on the **Server Section** details page.

{{< figure src="/images/automate/migration_is_in_progress_page.png" alt="Chef Server Details Page">}}

Once the parsing of the `.zip` file content is done, a preview link is shown beside the progress label terming **Sync in Progress**.  

{{< figure src="/images/automate/sync_in_progress.png" alt="Sync In Progress">}}

Select the **Click to Preview** link to view the screen.

{{< figure src="/images/automate/click_to_preview.png" alt="Click To Preview">}}

## Cancel Sync

To cancel the sync, select the **Cancel** button. The cancellation will not modify anything in the system.

{{< figure src="/images/automate/cancel_migration_button.png" alt="Cancel Migration Button">}}

## Previewing Sync

The preview screen gives the information related to organizations where you can view:

- The number of entire organizations to be updated.
- The number of organizations already available with Automate needs no modification or is skipped.
- The number of organizations to be deleted.

You can also view the list of users to be added to Automate on the preview screen. Refer to the **Link to User Permission** to learn about User Permission Sync.

{{< figure src="/images/automate/preview_screen.png" alt="Preview Screen">}}

The preview screen helps in letting you know which are available in Automate system associated with different Chef Server. Automate does not take any decisive action but gives a provision to the admin to provide a new **username**.
These users are highlighted, and a text box is enabled to add the corresponding new **username**.

## Confirming Sync

The sync will continue if you select the **Confirm** button. The Automate will show the **Compete** status when sync is completed.

{{< figure src="/images/automate/cancel_migration_button.png" alt="Confirm Sync Button">}}

## Failed Sync

The sync might fail and show the **Failed** status under metadata. You can learn about the reason for failure by hovering over the **Error** icon.

{{< figure src="/images/automate/chef-infra-server-migration-failed-metadata.png" alt="Failed Metadata">}}

## Output of Sync

- All the organizations are synced to Automate and linked to Chef Infra Server.
- Every organization has an **IAM** project associated.
- All the users from the Chef Server are synced.
- The users are added to the Chef Server and associated with the organization and the corresponding **IAM** projects.
- The users are added to the **IAM** policies of the IAM projects based on the permission user holds in Chef Server. Refer to the **Link to User Permission** to learn more about User Permission Sync.

## User Permission Sync

Automate on sync creates a unique project for every newly added organization. The different policies and their associated permissions are listed below:

| | Cook books | Roles | Environments | Attributes | Data bags | Clients | Nodes | Policy Files | Policy Group | Users under an Org | Add Infra Server |
|-|-----------|-------|--------------|------------|----------|---------|------|-------------|--------------|-------------------|------------------|
| Viewer | Read | Read | Read | Read | Read | Read | Read | Read | Read | No access | No access |
| Editor        | Read      | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | Read, Edit, Create         | No access          | No access        |
| Project Owner | Read      | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete         | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read, Edit, Create, Delete | Read               | No access        |

{{< note >}} The users under admin group in an organization are added to **Project Owner Policy**. {{< /note >}}

{{< note >}} The users under any group other than admin in an organization are added to **Editor Policy**. {{< /note >}}
