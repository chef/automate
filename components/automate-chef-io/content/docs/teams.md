+++
title = "Teams"
description = "Manage Chef Automate Teams."
date = 2018-05-16T16:03:13-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 60
+++

## Overview
This guide will show you how to manage Chef Automate teams. You can import existing teams into Chef Automate with [Microsoft AD (LDAP)]({{< ref "configuration.md#microsoft-active-directory" >}}), [generic LDAP]({{< ref "configuration.md#ldap" >}}), or [SAML]({{< ref "configuration.md#saml" >}}). You can also create local Chef Automate teams that are independent of LDAP or SAML.

Permission on the `iam:teams` action is required to interact with teams. Any user that is part of the `admins` team or the `Administrator` policy will have this permission. Otherwise, [IAM v2 custom policies]({{< relref "iam-v2-guide.md#creating-custom-policies" >}}) can be created to assign this permission.

## Managing Local Teams

### Creating Local Teams
Navigate to _Teams_ in the **Settings** tab. Then use the **Create Team** button, which opens a helper window for entering the team's _Name_, a unique _ID_, and optionally assign the team to some _Projects_.

![Create Local Team](/images/docs/admin-tab-teams-list.png)

### Deleting Local Teams
Navigate to _Teams_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete Team**.

### Adding Local Users to Teams
To add local users to a team, navigate to _Teams_ from the **Settings** tab and locate the team. Navigate to the team's page, and then use the **Add Users** button.

#### Admins Team
Chef Automate comes with an _Admins_ team. Local users can be added directly to this team, which assigns them admin permissions.

### Removing Local Users from Teams
To remove local users from a team, navigate to _Teams_ from the **Settings** tab and locate the team. Navigate to the team's page, locate the user to remove, then use the menu at the end of the table row to remove the user.

### Changing Team Details
Both the team name and the projects a team belongs to can be changed by navigating to _Teams_ from the **Settings** tab, selecting an individual team and then navigating to the **Details** tab.
