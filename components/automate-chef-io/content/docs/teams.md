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

This guide will show you how to manage Chef Automate teams. Import existing teams into Chef Automate with [Microsoft AD (LDAP)]({{< ref "configuration.md#microsoft-active-directory" >}}), [generic LDAP]({{< ref "configuration.md#ldap" >}}) or [SAML]({{< ref "configuration.md#saml" >}}).

You can create local Chef Automate teams that are independent of LDAP or SAML.Teams can be used for policy-based [authorization]({{< ref "authorization-overview.md" >}}).

## Prerequisites

Before you follow these instructions, we recommend you install the JSON processor [jq](https://stedolan.github.io/jq/) to ensure readable output. Without it, some commands may need to be modified.

You will need administrative access to interact with the teams API. An existing administrative user can provide that access.

To interact with the teams API using cURL, fetch an admin API token available from the `chef-automate` CLI, and set it to a usable variable:

```bash
export TOK=`chef-automate admin-token`
```

## Teams

### Creating Teams

#### Create a Team from Chef Automate

As an administrative user, you can create a team in the UI from the **Settings** tab. Select _Teams_ in the sidebar, and then use the **Create Team** button:

![Create Local Team](/images/docs/admin-tab-teams-list.png)

First, enter a unique name and description for the team. Save your new team:

![Create Team](/images/docs/admin-tab-team-create.png)

Upon creating the team, you'll be taken to the new team's details page:

![Team Details](/images/docs/admin-tab-team-details.png)

Add users to the new team:

![Add Users](/images/docs/admin-tab-team-add-users.png)

Edit the team name:

![Edit Team](/images/docs/admin-tab-team-edit.png)

Now, you can [create a new policy]({{< ref "authorization-overview.md#common-use-cases" >}}) for your team. All members will now have additional access based on that new policy.

#### Create a Team using the Command Line with cURL

To create a Chef Automate team, you'll need to provide a name and description. Team names must be unique.

```bash
curl -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"name":"Team Name", "description":"My Chef Team"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/teams?pretty
```

### Fetching Teams

You can fetch a team by its ID:

```bash
curl -H  "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{id}?pretty
```

You can also fetch all teams, collectively:

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams?pretty
```

### Updating Teams

To update a team, you must supply its name and description:

```bash
curl -X PUT -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"name":"An Updated Team Name", "description": "An updated description"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{ID}?pretty
```

### Deleting Teams

To delete a team, you must supply its ID:

```bash
curl -X DELETE -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{ID}
```

## Managing Chef Automate User and Team Associations

### Viewing a User's Teams

To view a user's teams, you will need the user's ID:

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/users/{user_ID}/teams?pretty
```

### Viewing a Team's Users

To view a team's users, you will need the team's ID. This returns JSON that contains an array of user IDs associated with the team:

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{team_ID}/users?pretty
```

### Adding Users to a Team

To add users to a team, you will need both the team ID and the IDs of the users you will add:

```bash
curl -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"user_ids":["userID", "secondUserID"]}' https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{team_ID}/users?pretty
```

### Removing Users from a Team

To remove users from a team, you will need both the team ID and the IDs of the users you will remove:

```bash
curl -X PUT -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"id":"teamID", "user_ids":["userID", "secondUserID"]}' https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/{team_ID}/users
```

## Common Use Cases

### Adding Users to the Admin Team

Adding users to the default `admins` team will give them full access to all endpoints in Chef Automate; they will be able to manage policies, users, teams, and integrations.

You may add users on the `admins` Team details page:

![Add Users to Admins](/images/docs/admin-tab-team-add-admins.png)

You may also complete this operation from the command line.

1. Fetch an admin API token available from the `chef-automate` CLI and set it to a usable variable:

    ```bash
    export TOK=`chef-automate admin-token`
    ```

1. Get the `admins` team ID and set it to a usable variable:

    ```bash
    export ID=`curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams | jq -r '.teams[] | select(.name =="admins").id'`
    ```

1. Confirm the user IDs for the user(s) you want to add to the `admins` team.

    ID of a single user:

    ```bash
    curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/users/{username} | jq .id
    ```

    Fetch all users (with IDs):

    ```bash
    curl -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/users?pretty
    ```

1. Add the user(s) to the `admins` team:

    ```bash
    curl -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"user_ids":["userID", "secondUserID]}' https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/$ID/users?pretty
    ```

1. Verify that the user is a member of the team by listing all members of the `admins` team:

    ```bash
    curl -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/teams/$ID/users?pretty
    ```
