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

This guide will show you how to manage Chef Automate users. Import existing users into Chef Automate with [Microsoft AD (LDAP)]({{< ref "configuration.md#microsoft-active-directory" >}}), [generic LDAP]({{< ref "configuration.md#ldap" >}}) or [SAML]({{< ref "configuration.md#saml" >}}).

You can create local Chef Automate users that can log in and interact with the system independent of LDAP or SAML.

## Prerequisites

You will need administrative access to interact with users other than yourself. An existing administrative user can provide that access. If you are already an administrative user, you can [create users in the UI]({{< relref "#with-the-browser" >}}) by logging into Chef Automate with your admin credentials.

## Users

Chef Automate supports three different types of users: local users, [LDAP users]({{< relref "ldap.md" >}}), and [SAML users]({{< relref "configuration.md#saml" >}}). Manage local users from the **Settings** tab.

### Manage Local Users from the UI

Navigate to _Users_ in the **Settings** tab.

To add a local user, use the **Create User** button, which opens a helper window for entering the user's _full name_, a unique _username_, _password_ and _confirm password_.
Once you've filled in the information, use the **Save and Close** button.

![Add Local User](/images/docs/admin-tab-users-list.png)

To change or delete a user account, select their name from the _Users_ page. You can also delete users from the _Users_ page by using the menu at the end of the table row.

![Modify Local User](/images/docs/admin-tab-user-edit.png)

### Manage Local Users from the Command Line with cURL

Before you follow these instructions, we recommend you install the JSON processor [jq](https://stedolan.github.io/jq/) to ensure readable output. Without it, some commands may need to be modified.

To interact with the user API using cURL, fetch an admin API token available from the `chef-automate` CLI, and set it to a usable variable:

```bash
export TOK=`chef-automate admin-token`
```

#### Create a User

To create a Chef Automate user, you'll need a name, username, and password.
The username must be unique.

```bash
curl -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"name":"Your Name", "username":"username001rulez", "password":"password"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/users?pretty
```

#### Fetching Users

You can fetch a single user by username. Keep in mind that certain characters
in a username (such as a space) may need to be escaped in the URL.

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/users/username001rulez?pretty
```

More generally, here is the format showing a `{username}` placeholder:

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/users/{username}?pretty
```

You can also fetch a list of all users by omitting the final username segment of the URL:

```bash
curl -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/users?pretty
```

#### Updating Users

You can update a user's full name (`name` property) and/or password (`password` property).
To identify the proper user record, supply the username in the URL _and_ the user's `id` in the payload.
Then, also in the payload, you must specify the full name--_even if you do not want to change it!_
Finally, include the password in the payload, but only if you do want to change it.

```bash
curl -X PUT -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"name":"Revised Full Name", "id": "userID", "password": "another_pwd"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/users/{username}?pretty
```

A non-admin user is also able to change their own password through the UI.
For completeness, here is the API call to perform the same action.

```bash
curl -X PUT -H "api-token: $TOK" -H "Content-Type: application/json" -XPUT -d'{"id":"userID","name":"Revised Full Name","username":"username001rulez","password":"another_pwd","previous_password":"password"}' https://{{< example_fqdn "automate" >}}/api/v0/users/{username}?pretty
```

### Deleting Users

To delete a user, supply the `username`:

```bash
curl -X DELETE -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/users/{username}?pretty
```

### User Self-Maintenance

Local Automate users can manage their own name and password through the Chef Automate user interface.
Select the user icon in the top navigation bar,
then select **Profile** from the drop-down.

![Navigate to user profile](/images/docs/user-profile-navigation.png)

The sidebar should reflect **Your Profile** as the active panel,
and you should see your user name,
your avatar (if your username is your email address), and your full name.
Use the **Edit** button to edit your full name,
while the lower portion of the page allows you to update your password.

![View user details](/images/docs/user-profile-view.png)
