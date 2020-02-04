+++
title = "Projects"
description = "IAM Projects"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 100
+++

## Overview
Identity and Access Managment projects allow for filtering and segregation of your data amongst your user base.

Permission on the `iam:projects` action is required to interact with projects.

## Managing Projects

### Creating Projects
Navigate to _Projects_ in the **Settings** tab. Then use the **Create Project** button, which opens a helper window for entering the project's _Name_. We automatically generate a project ID for you, if you would like to change it use the **Edit ID** button.

When a project is created, 3 policies that control access to that project are also created. Those policies inlude: _Project Owner_, _Project Editor_, and _Project Viewer_. Adding members to these policies will grant them access to the project.

![](/images/docs/settings-projects.png)

### Deleting Projects
Only projects with zero ingest rules and no pending edits can be deleted. To delete a project navigate to _Projects_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete Role**.

### Changing Project Details
#### Ingest Rules
Ingest rules allow ingested events and nodes to be added to projects. *Node* corresponds to ingested client run and compliance nodes, and *Event* corresponds to ingested events on the _Event Feed_ page. 

Ingest rules can be created or changed by navigating to _Projects_ in the **Settings** tab and then selecting a project. When you create or update ingest rules, those changes are staged and **not** directly applied.
Other users may also stage changes.

Each rule describes a list of **conditions**, where each condition describes a single characteristic.

##### Project Ingest Rule Conditions

A condition consists of these properties:

Property               | Description
-----------------------|------------
Event Attribute        | Chef Organization or Chef Server
Node Attribute         | Chef Organization, Chef Server, Environment, Chef Role, Chef Tag, Chef Policy Name, or Chef Policy Group
Operator               | equals of member of
Values                 | list of one or more values to match on the specified attribute

#### Details
To change the name of a project navigate to _Projects_ in the **Settings** tab and then select the **Details** tab.

### Updating Projects
The _Project List_ page displays the status of project ingest rules (*No rules*, *Edits pending*, or *Applied*).

If a project has pending edits from changes to ingest rules then all projects must be updated for those pending edits to take effect. The update background process can take up to 12 hours if there is a lot of historical data. 

All changes will be applied together when you update projects. To update projects naigate to _Projects_ in the **Settings** tab and use the **Update Projects** button.

#### Stopping the Project Update Early
Stopping a project update early will leave your resources in a bad state. Some resources will be in the correct projects and others will not be. To resolve this, make sure to start another project update.

To stop the project update background process before it finishes naigate to _Projects_ in the **Settings** tab and use the **Stop Updating Projects** button.
