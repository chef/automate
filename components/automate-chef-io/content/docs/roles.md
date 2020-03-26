+++
title = "Roles"
description = "IAM Roles"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 90
+++

## Overview

Chef Automate Identity and Access Management roles are named groups of actions used to define [policies]({{< relref "policies.md" >}}). Actions describe what is allowed by users in Automate.

Permission for the `iam:roles` action is required to interact with roles. Any user that is part of the `admins` team or the `Administrator` policy will have this permission. Otherwise, [IAM custom policies]({{< relref "iam-v2-guide.md#creating-custom-policies" >}}) can be created to assign this permission.

![](/images/docs/settings-roles.png)

### Chef-Managed Roles

Chef-managed roles are roles provided by Chef that cannot be changed.

Role          | Description
--------------|------------
Viewer        | **View** everything in the system *except* IAM
Editor        | **Do** everything in the system *except* IAM and license application
Owner         | **Do** everything in the system *including* IAM
Project Owner | Editor + **view** and **assign** projects
Ingest        | Ingest data into the system

### Custom Roles

Custom roles are roles that can be changed by anyone with the permission for `iam:roles:update`.

## Managing Roles

### Creating Roles

_Custom_ roles can only be created using the [Roles API]({{< relref "api/#tag/roles" >}}).

### Changing Role Details

For _custom_ roles, the role name, actions list, and projects can only be changed using the [Roles API]({{< relref "api/#tag/roles" >}}).

### Deleting Roles

Navigate to _Roles_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete Role**.
