+++
title = "Policies"
description = "IAM Policies"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 80
+++

## Overview
Identity and Access Managment policies manage the resources and actions used by identities. Policies are composed of statements that specify permissions. 

Permission on the `iam:policies` action is required to interact with policies. Any user that is part of the `admins` team or the `Administrator` policy will have this permission. Otherwise, [IAM v2 custom policies]({{< relref "iam-v2-guide.md#creating-custom-policies" >}}) can be created to assign this permission.

![](/images/docs/settings-policies.png)

### Chef-Managed Policies
*Chef-managed* policies are provided by Chef and are integral to the operation of Chef Automate. The policy statements in Chef-managed policies cannot be changed.

### Custom Policies
*Custom* policies are policies that you create for your own needs. You can add, edit, and delete policy statements in your custom policies.

## Managing Policies

### Creating Policies
_Custom_ policies can only be created using the [Policies API]({{< relref "api/#tag/policies" >}}).

### Deleting Policies
Navigate to _Policies_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete Policy**.

### Policy Membership
The policy membership can be changed for both _Chef-Managed_ and _Custom_ policies. The only exception is that the _admins_ team cannot be removed from the _Administrator_ policy.

#### Adding Members to Policies
To add members to a policy, navigate to _Policies_ in the **Settings** tab and locate the policy. Navigate to the policy's detail page and use the **Add Members** button. Select local users or teams from the list, or use the **Add Member Expression** button to add API Tokens, and SAML or LDAP users or groups.

#### Removing Members from Policies
To remove members from a policy navigate to _Policies_ in the **Settings** tab and locate the policy. Navigate to the policy's detail page and select the **Members** tab. Then locate the member to remove and use the menu at the end of the table row to remove the user.

### Changing Policy Details
For _custom_ policies the policy name, statements, and projects can only be changed using the [Policies API]({{< relref "api/#tag/policies" >}}).
