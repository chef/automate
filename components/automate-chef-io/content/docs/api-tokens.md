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

Chef Automate has two different types of API tokens: administrative and standard.
With an administrative token you can access the entire Chef Automate API--including administrative tasks such as [managing local users]({{< relref "users.md" >}}) and [managing local teams]({{< relref "teams.md" >}}) and [managing authorization policies]({{< relref "authorization-overview.md" >}}).
Standard tokens have much more limited permissions; they are designed for Chef Clients and InSpec Agents to send data to Chef Automate.
You can also use a standard token to access any part of the Chef Automate API if you write a policy granting it specific access. However, granting a user access to the ``auth:tokens`` resource also gives that user access to the administrative token.

Before you follow these instructions, we recommend you install the JSON processor [jq](https://stedolan.github.io/jq/) to ensure readable output. Without it, some commands may need to be modified.

## Viewing API Tokens

![API Tokens](/images/docs/admin-tab-API-tokens-list.png)

From the **Settings** tab, navigate to the _API Tokens_ page. Admin users and non-admin users with admin-like authorization defined in policies, will see all of the tokens registered in Chef Automate.

For more information on defining policies, see [default policies]({{< relref "default-policies.md" >}}).

## Managing API Tokens

### The `admin-token` and Global API Access

Create the Admin API token granting global API access from the command line. Follow the steps in [Creating an Admin API token]({{< relref "#creating-an-admin-api-token" >}}). If you've already created an Admin token, you will see it in the API token list.

## Creating a Standard API Token

Create standard API tokens with limited API access levels, or for use with Chef Client and Inspec data reporting, using either the user interface or the [API]({{< relref "api-tokens.md#creating-a-standard-api-token" >}}).
By default, standard tokens have access only to data collection API endpoints, so to use them outside the context of Chef Clients and InSpec Agents, you'll need to [create a policy]({{< relref "authorization-overview.md#default-authorization-settings" >}}) for your token.

You must log in as an Admin to create or modify standard tokens from the browser. Open the **Settings** tab and navigate to the _API Tokens_ page.

Selecting the **Create Token** button opens a pop-up. Enter a name for your token into the form and select the **Create Token** button on the form to submit the information. Token names can contain lower-case letters, hyphens, and numbers, but they cannot contain symbols or upper-case letters. In some situations, you may prefer to set the token ID to something different than the name. In this case, select "Edit ID" on the form and edit the token ID in the form.
After selecting the **Create Token** button that closes the form,
your token will appear in the API Tokens table.

Copy the token to your clipboard, toggle the status, or delete the token using the menu at the end of the table row. Change the token name by selecting the current token name on the API Tokens table, which opens a token details page. Edit the token in the name field, and select the **Save** button. Return to the API Tokens table by selecting _API Tokens_ from the navigation panel or from the top of the page.

![API Token Create](/images/docs/admin-tab-API-token-create.png)

By default, these tokens only have access to data collection API endpoints.
To use them outside the context of Chef Clients and InSpec Agents, you'll need to [create a policy]({{< relref "authorization-overview.md#default-authorization-settings" >}}) for your token.
For more information on how to permission a standard API token,
see [Permissioning a Standard API Token]({{< relref "#permissioning-a-standard-api-token" >}}).

## Token Management from the Command Line

### Creating a Standard API Token via the API

If you already [created an Admin API token]({{< relref "#creating-an-admin-api-token" >}}), you can create a standard API token via the API.
You can give it a description to denote its use:

```bash
curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"description":"My shiny new token"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/tokens | jq .id
```

## Creating an Admin API Token

To create an Admin API token with global access to the API, you'll need to log onto your Chef Automate
installation and use the `chef-automate` CLI command:

```bash
chef-automate admin-token
```

This command outputs your new token to the terminal.

To create an admin token and immediately store it in an environment variable for
easy access, you can instead run:

```bash
export TOK=`chef-automate admin-token`
echo $TOK
```

Once you have an Admin API token, you can use it to make requests by passing it in the `api-token`
header:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies -v
```

If you have Admin level access to the API, you can retrieve your token at any time by going to
`https://{{< example_fqdn "automate" >}}/settings/tokens` and finding the token with the description:

```text
This token was generated by the chef-automate CLI tool. It has admin level access on the entire Chef Automate API.
```

## Permissioning a Standard API Token

Grant your API tokens access to some or all of the API by creating a policy for the token.
One common example of creating a policy is granting a client's API token
access to all of the Compliance API.
See [Authorization]({{< relref "authorization-overview.md" >}}) for more information about authorization and policies.

{{% warning %}}
Granting a user access to the ``auth:tokens`` resource also gives that user access to all tokens,
including the administrative token.
{{% /warning %}}

> As an admin, I would like to create a token that gives a client permission to read any
compliance resource.

1. [Get an Admin API token]({{< relref "#creating-an-admin-api-token" >}}) and save it in the
   environment variable `$TOK`:

   ```bash
   export TOK=<your_admin_api_token>
   ```

2. [Create a standard API token]({{< relref "#creating-a-standard-api-token" >}}) to permission.

3. Copy the token returned.

4. Create policies to permit that client to read `compliance:*`. For more information, see [policies]({{< relref "authorization-overview.md" >}}).

   ```bash
   export TOK=<your_admin_api_token>
   curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["token:95aef20b-0a4e-4698-bd69-ce2cf44c2e35"], "action":"read", "resource":"compliance:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
   ```
