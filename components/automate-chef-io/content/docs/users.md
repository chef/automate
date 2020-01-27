+++
title = "Users"
description = "Manage Chef Automate Users."
date = 2018-05-16T16:03:13-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 50
+++

## Overview

Chef Automate supports three different types of users: local users, [LDAP users]({{< relref "ldap.md" >}}), and [SAML users]({{< relref "configuration.md#saml" >}}). Manage local users from the **Settings** tab.

Local users can sign in and interact with the system independent of LDAP or SAML.

Permission on the `iam:users` resource is required to interact with users other than yourself. Any user that is part of the `admins` team, or the `Administrator` policy will have this permission, otherwise, [IAM v2 custom policies]({{< relref "iam-v2-guide.md#creating-custom-policies" >}}) can be created to assign this permission.

## Managing Local Users

### Creating Local Users

Navigate to _Users_ in the **Settings** tab. Then use the **Create User** button, which opens a helper window for entering the user's _display name_, a unique _username_, _password_, and _password confirmation_.

![Add Local User](/images/docs/admin-tab-users-list.png)

### Deleting Local Users

Navigate to _Users_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete User**.

### Resetting Passwords

Navigate to _Users_ in the **Settings** tab and locate the user who needs a password reset. Navigate to their user page, and then the **Reset Password** tab. Provide a new password and confirm the new password, then select the **Reset Password** button.

All local users can also reset their own passwords.

### Changing Display Names

Navigate to _Users_ in the **Settings** tab and locate the user who needs their display name changed. Navigate to their user page, provide a new display name and select the **Save** button.

All local users can also change their own display names.

## User Self-Maintenance

Local Automate users can manage their own display name and password.
Select the user icon in the top navigation bar,
then select **Profile** from the drop-down.

![Navigate to user profile](/images/docs/user-profile-navigation.png)
