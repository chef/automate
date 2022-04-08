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

The _Chef Infra Server Org and User Migration_ page (**Infrastructure > Chef Servers > Chef server details**) lets you connect existing Chef Infra Server orgs and users to Chef Automate, migrate all the orgs and user into the automate using the simple migration process.


### Chef Server Org and User Migration steps

Empty list of orgs and users on server details page.

{{< figure src="/images/automate/chef_server_details_page.png" width="500" alt="Chef Server Details Page">}}

To migrate existing orgs and users to the Chef Automate infrastructure, go to server details page inside the metabox click on `sync org and users` button.

When it is clicked, a slider opens from the right.

{{< figure src="/images/automate/sync_org_and_user_slider.png" width="500" alt="Chef Server Details Page">}}

On `sync org and users slider`, Upload a zip file (either select the zip file or drag and drop the zip file)
after selecting a zip file, click on `upload file` button

{{< figure src="/images/automate/select_zip_file_page.png" width="500" alt="Chef Server Details Page">}}

Once click on the `upload file` button, zip file will uploaded and slider will close.

Then the server details page show you last successfull migration date and last migration status under the metabox, if metbox show you like `Sync in progress` which means, migration is in progress and how many steps are completed.

{{< figure src="/images/automate/migration_is_in_progress_page.png" width="500" alt="Chef Server Details Page">}}

On right side of `Sync in progress` one preview link is available with name `click to preview`, after clicking on this link a new slider (preview screen) will open. where you can see count of migrating orgs and the list of users which are going to migrate. By default, all users are unchecked. if a particular user needs to be migrated, its checkbox needs to be clicked.

### conflicting user scenerio:
sometimes some users are already exist, in that case migration slider will show you the input for indiviual user to change the automate username. 

- need to add the screenshot

after the user selection you can click to `confirm`, then you final stage of migration process will start and finally orgs and users are migrated into the automate.
migration is complete after migration process done.

- need to add the screenshot

### cancel migration:
clicking to `cancel` button on `preview screen` will cancel the migration, same status will show under the metabox `Last sync status`

- need to add the screenshot

### Failed migration: 

In migration process, if something goes wrong and migration is not completed, in that case migration is failed. same status will show under the metabox `Failed`, we can also see the error message, on hovering the error icon.

- need to add the screenshot
