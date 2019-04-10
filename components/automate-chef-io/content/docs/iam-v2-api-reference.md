+++
title = "IAM v2 API Reference"
description = "API reference guide for IAM v2"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "beta"
    weight = 30
+++

This guide shows you how to manage Chef Automate IAM v2 policies and roles from the command line.
Any command with a browser equivalent includes brief directions for locating it on the corresponding page.
See the [IAM v2 Overview]({{< relref "iam-v2-overview.md" >}}) for an overview and exposition of new concepts.
See the [IAM v2 User's Guide]({{< relref "iam-v2-guide.md" >}}) for more information on setting up and using IAM v2 features.

## Prerequisites

1. Upgrade to IAM v2 using the [IAM v2 User's Guide]({{< relref "iam-v2-guide.md#upgrade-to-iam-v2" >}}).

2. _Optional_ Install the [jq](https://stedolan.github.io/jq/) JSON processor to ensure readable output.

3. Generate a new admin token for IAM v2.
   (The v1 command `chef-automate admin-token` will not work for v2, though any existing v1 admin tokens will still work.) Use:

   ```bash
   export TOK=`chef-automate iam token create TEST_ADMIN --admin`
   ```

## Policies

### Listing Policies

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies | jq
```

The output includes both `Chef-managed` and `custom` policies.
Policies migrated from IAM v1 are included as `custom` policies.
Policy names that begin with `[Legacy]` are default policies migrated from v1.
We recommend deleting these legacy policies and setting up new IAM v2 policies in their place.

In the browser: Navigate to the *Policies* page under the **Settings**  tab.

### Getting a Policy

In order to fetch a policy on the command-line, you must know its ID.
This may be obtained by first fetching the list of all policies; see [Listing Policies]({{< relref "iam-v2-api-reference.md#listing-policies" >}}).
Find the `id` field for the policy of interest--for example, the ID from the default administrator policy is `administrator-access`.
Plug that in to the URL as shown:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/administrator-access | jq
```

In the browser: Navigate to the *Policies* page under the **Settings**  tab, and then select an individual policy to open its details.

### Creating a Policy

Create a policy by composing JSON with all the necessary policy data.

This example policy is made up of two statements, each enforcing a different set of permissions.
The first policy statement grants users list view access.
The second statement grants editor access via the `editor` role.
You can see the actions contained in the `editor` role in the [Roles section]({{< relref "iam-v2-api-reference.md#roles" >}}).

```json
{
  "name": "Test Policy",
  "id": "test-policy-1",
  "members": ["team:local:alpha","team:ldap:beta"],
  "statements": [
    {
      "effect": "ALLOW",
      "actions": [
        "iam:users:list"
      ]
    },
    {
      "effect": "ALLOW",
      "role": "editor"
    }
  ]
}
```

Assuming you stored the above JSON in the file "policy.json", pass it to `curl` to create the policy:

```bash
curl -s -H "api-token: $TOK" -d @policy.json https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies | jq
```

### Updating a Policy

In the IAM beta, policies must be updated from the command line.

* You can modify the *membership* of both Chef-managed and custom policies; see [Policy Membership]({{< relref "iam-v2-api-reference.md#policy-membership" >}}) for more information.
* You can modify the *definition* of custom policies but not Chef-managed policies.
* You can change the policy name, members, or statements, but you cannot change the policy ID.
* Modifying any part of a policy resets *all* of its properties.
  Properties that aren't included in the command are reset to empty values.
  Thus, you must supply all of a policy's properties, not just the ones you wish to update.

In order to update a policy, you must know its ID.
Use the command from [Listing Policies]({{< relref "iam-v2-api-reference.md#listing-policies" >}}) to locate your policy ID.

The simplest way to pass a policy is by storing it in a file; we used a sample `policy.json` in the previous section.
To update that policy you still need to pass in the complete policy definition--plus its members--just as you did when creating it; any omitted properties will be erased from the policy!

As an example, to update the example policy to keep the first policy statement allowing users list-access, while removing `editor` access, edit your `policy.json` file:

```json
{
  "name": "Test Policy",
  "id": "test-policy-1",
  "members": ["team:local:alpha","team:ldap:beta"],
  "statements": [
    {
      "effect": "ALLOW",
      "actions": [
        "iam:users:list"
      ]
    }
 ]
}
```

To update your policy, replace the last component of the path in the command with the ID of the target policy.

```bash
curl -s -H "api-token: $TOK" -d @policy.json -X PUT https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id} | jq
```

You can also make the change specifying the data inline:

```bash
curl -s -H "api-token: $TOK" -d '{"name":"Test Policy", "id":"test-policy", "members":["team:local:alpha","team:ldap:beta"], "statements": [{"actions": [ "iam:users:list"], "effect": "ALLOW"}]}' -X PUT https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id} | jq
```

### Policy Membership

Chef Automate supports two methods for updating policy membership.

Using PUT lets you **set** membership for the entire policy; see [Setting Policy Membership]({{< relref "iam-v2-api-reference.md#setting-policy-membership" >}}).

Using POST lets you add members to (or remove members from) an existing membership list.
For more information, see [Adding Members to Policies]({{< relref "iam-v2-api-reference.md#adding-members-to-policies" >}}) and [Removing Members from Policies]({{< relref "iam-v2-api-reference.md#removing-members-from-policies" >}}).

#### Setting Policy Membership

From the command line, specifying a complete list of members is often simpler than keeping track of a policy's membership and adding or deleting specific members (whether users, team, or tokens).
This command *replaces* the policy's membership.

```bash
curl -s -H "api-token: $TOK" -X PUT -d '{"members":["team:local:admins","token:admin-token"]}' https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id}/members | jq
```

#### Adding Members to Policies

The `POST` operation adds new members to the existing set of members, rather than replacing it, so you only need to supply the new members. Building on the previous example, after using this command, the policy membership includes the `admins` team, a token, and the `editors` team.
The `members` property must be an array argument, even if supplying only one member.
Note that both the HTTP method (PUT above and POST here) and the URL are different than the command used for [setting policy membership]({{< relref "iam-v2-api-reference.md#setting-policy-membership" >}}).

```bash
curl -s -H "api-token: $TOK" -X POST -d '{"members":["team:local:editors"]}' https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id}/members:add | jq
```

In the browser: Navigate to the *Policies* page under the **Settings** tab. and select an individual policy to open its detail view.
Select the **Members** to view the current membership.
Use the **Add Members** button to open a list of candidate members.
Select any members that should be added to the policy.
Use the **Add # Members** button to complete the operation.

#### Removing Members from Policies

This example removes the `team:local:admins` team from the policy.
(The removed member still exists within Chef Automate, but no longer has the permissions derived from this policy.)
Note that the `members` property must be an array argument, even if only supplying one member.

```bash
curl -s -H "api-token: $TOK" -X POST -d '{"members":["team:local:admins"]}' https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id}/members:remove | jq
```

In the browser: Navigate to the *Policies* page under the **Settings** tab., and then select an individual policy to open its detail view.
Select its **Members** tab to view the current membership.
Identify the member you wish to remove from the policy, open the menu at the end of its row, and then select the **Remove Member** option.

### Deleting a Policy

In the IAM v2 beta, policies must be deleted from the command line.

See [Listing Policies]({{< relref "iam-v2-api-reference.md#listing-policies" >}}) to fetch a list of all your policies and locating the relevant policy ID.
Use the policy ID as the final path component to delete a policy:

```bash
curl -s -H "api-token: $TOK" -X DELETE https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies/{policy-id} | jq
```

In the browser: Navigate to the *Policies* page under the **Settings** tab., and then find the policy you wish to delete.
Use the menu at the end of the policy row and select the **Delete Policy** option to delete the policy.

## Roles

A role encapsulates a list of actions.
IAM v2 provides several default Chef-managed roles, which are essential to the operation of Chef Automate and cannot be altered.
Roles you create are Custom roles.

Chef-managed Role Name | ID| Actions
-----------------------|-----|--------
Owner       | owner       | `*`
Viewer      | viewer      | `infra:*:get`, `infra:*:list`, `compliance:*:get`, `compliance:*:list`, `system:*:get`, `system:*:list`, `event:*:get`, `event:*:list`, `ingest:*:get`, `ingest:*:list`
Editor      | editor      | `infra:*`, `compliance:*`, `system:*`, `event:*`, `ingest:*`, `secrets:*`, `telemetry:*`
Ingest      | ingest      | `infra:ingest:*`, `compliance:profiles:get`, `compliance:profiles:list`

### Listing Roles

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/roles | jq
```

The output details each role name, role type, and actions it contains.

In the browser: Navigate to the *Roles* page under the **Settings** tab.

### Getting a Role

Roles have unique IDs.
For example, the Editor role's ID is `editor`.
This may be obtained by first fetching the list of all roles; see [Listing Roles]({{< relref "iam-v2-api-reference.md#listing-roles" >}}).
Use the role ID as the final path component to fetch a role:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/roles/editor | jq
```

In the browser: Navigate to the *Roles* page under the **Settings** tab, and then select an individual role to open its detail view.

### Creating a Role

In the IAM v2 beta, custom roles must be created on the command line.
Use the following `curl` command to create a role combining some editor actions with some administrator actions:

```bash
curl -s -H "api-token: $TOK" -d '{"name": "Test Role", "id": "test-role-1", "actions": ["infra:*", "compliance:*", "teams:*", "users:*"] }' https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/roles | jq
```

### Updating a Role

In the IAM v2 beta, custom roles must be updated on the command line.
Use the following command to update the previously created role to remove the action `infra:*`:

```bash
curl -s -H "api-token: $TOK" -d '{"name": "Test Role", "actions": ["compliance:*", "teams:*", "users:*"] }' -X PUT https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/roles/test-role-1 | jq
```

It is important to note that for the `actions` property in the JSON you must specify the complete list of actions.
Contrast this with managing members, where you specify only the members to be added ([Adding Members to Policies]({{< relref "iam-v2-api-reference.md#adding-members-to-policies" >}}))

### Deleting Roles

In the IAM v2 beta, custom roles must be deleted on the command line.
Use the following command to delete the previously created role.
Use the role ID as the final path component to delete a role:

```bash
curl -s -H "api-token: $TOK" -X DELETE https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/roles/test-role-1 | jq
```

## Tokens

Tokens are unchanged for IAM v2.
To specify tokens in a [member expression]({{< relref "iam-v2-overview.md#members-and-policies" >}}), first find that token's ID.

### Listing Tokens

List all tokens with this command:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/tokens | jq
```

## Restoring Admin Access

While we have [safeguards]({{< relref "iam-v2-overview.md#policy-types" >}}) to prevent it, it is possible to lock yourself out of Chef Automate.
If you have root access to the node where Chef Automate is installed, use the following commands to restore admin access:

<!-- Wording/Replace password/replace token/v1 commands won't work on c2 -->

* This command, which is also available on IAM v1, resets the local `admin` user's password and ensures that user is a member of the local `admins` team, which is a permanent member of the Chef-managed `Administrator` policy.

```bash
  chef-automate iam admin-access restore NEW_PASSWORD
```

* Generate a new token and add that token as a new member of the Chef-managed `Administrator` policy. This is the equivalent of the v1 command `chef-automate admin-token`.

```bash
  chef-automate iam token create NAME --admin
```
