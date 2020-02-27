+++
title = "API Tokens"
description = "Create and Use API Tokens"
draft = false
bref = "Create and Use API Tokens"
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 70
+++

## Overview

API Tokens are used to access the Chef Automate API. They are the only way to authenticate against the Chef Automate API. Tokens can be added as members of policies in order to grant them permissions.

Permission for the `iam:tokens` action is required to interact with tokens. Any user that is part of the `admins` team or the `Administrator` policy will have this permission. Otherwise, [IAM v2 custom policies]({{< relref "iam-v2-guide.md#creating-custom-policies" >}}) can be created to assign this permission.

## Managing API Tokens

### Creating API Tokens

Navigate to _API Tokens_ in the **Settings** tab. Then use the **Create Token** button, which opens a dialog box for entering the token's _name_ and optionally assigning the API token to some _Projects_. A team ID automatically generates upon creation. If you would like to change the team ID, select the **Edit ID** button.

After a token is created from the UI, it will have no permissions. To assign the token permissions, navigate to _Policies_ in the **Settings** tab, locate the appropriate policy, and then add the token as a member of the policy using a [member expression]({{< relref "iam-v2-guide.md#member-expressions" >}}).

![API Tokens](/images/docs/admin-tab-API-tokens-list.png)

#### API Token Value

After creating an API Token, you can obtain the token's value by opening the menu at the end of the table row and selecting **Copy Token**.

#### Admin Tokens

Admin tokens are tokens that are automatically added to the Administrator policy, which grants full access to Chef Automate. 
Admin tokens can only be created using the `chef-automate` command line.

```
chef-automate iam token create <your-token-name> --admin
```

To create an admin token and immediately store it in an environment variable for easy access, you can instead run:

```bash
export TOKEN=`chef-automate iam token create <your-token-name> --admin`
echo $TOKEN
```

Once you have an Admin API token, you can use it to make requests by passing it in the `api-token` header:

```bash
curl -s -H "api-token: $TOKEN" https://{{< example_fqdn "automate" >}}/apis/iam/v2/policies -v
```

### Deleting API Tokens

Navigate to _API Tokens_ in the **Settings** tab. Then open the menu at the end of the table row and select **Delete Token**.

### Changing API Token Details

The API token name, projects the token belongs to, and the token's status can be changed by navigating to _API Tokens_ from the **Settings** tab, selecting an individual token and then navigating to the **Details** tab.
