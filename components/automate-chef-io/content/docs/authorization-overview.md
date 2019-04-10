+++
title = "Authorization Overview"
description = "Set up authorization on Chef Automate"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authorization"
    weight = 10
+++

This guide helps you understand and use Chef Automate's authorization system.

## Overview

Chef Automate uses a policy-based authorization system.
A policy defines which **actions** a user, team, or client (**subject**) may perform
on a **resource**.
Without a policy, a user cannot perform the desired action upon the resource.

For example, the following policy permits both the user with
the email `user@example.com` and members of the LDAP team `ops`
to `read` the profiles collected under the `compliance` namespace:

```bash
{
    "action": "read",
    "resource": "compliance:profiles",
    "subjects": [
        "user:local:user@example.com",
        "team:ldap:ops"
    ]
}
```

Policies can contain wildcards to match a range of values.
For example, this policy uses wildcards to allow any user to perform any action
on any resource within the `compliance` namespace:

```bash
{
    "action": "*",
    "resource": "compliance:*",
    "subjects": [
        "*"
    ]
}
```

Chef Automate generates a query against the defined policies every time
a subject tries to act on a resource.
For example, in this query the user `john@example.com`
would like to `read` the resource `cfgmgmt:nodes`:

```text
subjects: [user:local:john@example.com]
action: read
resource: cfgmgmt:nodes
```

The system issues that query when the user tries to access the `/nodes` page of Chef Automate.
If it matches against at least one policy, the action is allowed to proceed.

## Structure

A policy is made up of three essential components: the **subjects**, which is
a list of users and teams, the **action** that those users/team members are allowed to perform,
and the **resource** on which they perform actions. Resources can be very granular,
but for most instances, restricting at the uppermost namespace will suffice.
A command to create a policy has this form:

```bash
curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["subject1", "subject2"], "action":"whatever_action", "resource":"whatever_resource"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
```

Here is a concrete example, granting `read` permission for the user `user@example.com`
and for the members of `team:ldap:ops` on resource `cfgmgmt:nodes:*`:

```bash
curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["user:local:user@example.com"], "action":"read", "resource":"cfgmgmt:nodes:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
```

### Action

An `action` is the operation that a subject performs.
Examples include `create`, `read`, `upload` and `mark-deleted`.
This policy allows `user@example.com` to `read` the `compliance:profiles` namespace;
however, the user cannot perform any other actions on it:

```bash
{
    "action": "read",
    "resource": "compliance:profiles",
    "subjects": [
        "user:local:user@example.com"
    ]
}
```

On the other hand, a [wildcard]({{< relref "#wildcards" >}}) in the `action` field
permits _any_ action on the resource.

### Subjects

The `subjects` field of a policy contains an array of individual subjects. A subject can be:

* A user, identified by provider and email (`user:local:foo@bar.com`)
* A team, identified by provider and name (`team:local:the_foos`)
* An API token identified by ID (`token:1234-5678-9785`)

Examples:

```text
user:local:foo@bar.com
user:ldap:foo@bar.com
user:saml:foo@bar.com
team:local:the foos
team:ldap:ops_team
team:saml:dbas
token:1234-5678-9785
```

A wildcard can replace any term of the subject; however, no terms can follow the wildcard. Conceptually:

* `*`
* `term:*`
* `term:second_term:*`

Examples:

* any user (or similarly any team) of a specific provider: `user:ldap:*`
* any team (or similarly any user) at all: `team:*`
* anyone with an API token: `token:*`
* any requestor at all: `*`

Each user or team must specify its _provider_.
Automate currently supports LDAP, SAML,
and local users or teams. Here are team examples using all three:

```text
team:local:the_foos
team:ldap:ops_team
team:saml:dbas
```

You can create local teams via the Chef Automate command line.
For more information, see [creating teams]({{< ref "teams.md#creating-teams" >}}).

{{< warning >}}
The group configuration settings for LDAP, MSAD, and SAML are optional. If groups are not configured,
users authenticating via any of those identity providers will not be members of any teams.
So user permissions cannot be enforced by policies with teams for subjects.
{{< /warning >}}

### Resource

An `action` is performed on a `resource`.
Some types of resources are `nodes`, `compliance`, and `compliance:profiles`.

Resources are defined in hierarchical terms of increasing granularity, and may have any number of terms.
With a wildcard (`*`), you can define policies with permission on any tier of the hierarchy.

Examples:

```text
[ 1] *
[ 2] cfgmgmt
[ 3] cfgmgmt:*
[ 4] cfgmgmt:nodes
[ 5] cfgmgmt:nodes:*
[ 6] cfgmgmt:nodes:<some node ID here>
[ 7] cfgmgmt:nodes:<some node ID here>:*
[ 8] cfgmgmt:nodes:<some node ID here>:runs
[ 9] cfgmgmt:nodes:<some node ID here>:runs:*
[10] cfgmgmt:nodes:<some node ID here>:runs:<some run ID here>
```

For example, consider this policy that uses one resource from the above list:

```bash
{
    "action": "read",
    "resource": "cfgmgmt:nodes:*",
    "subjects": [
        "user:local:user@example.com"
    ]
}
```

Here, the user `user@example.com` may read any node resource under `cfgmgmt:nodes:*`,
such as `cfgmgmt:nodes:23`, `cfgmgmt:nodes:23:runs`, or `cfgmgmt:nodes:23:runs:123-31234-332`.

### Wildcards

This section covers the creation of more complex policies using wildcards.

You can use a wildcard in two ways:

* as a standalone action, resource, or subject: `*`
* as the final term in an action, resource, or subject: `cfgmgmt:nodes:*`

A policy with a wildcard permits access to any action, resource, or subject in that space.

Authorization permits wildcards in policy definitions, so you can create a policy covering
a range of possible values.
For example, to define a policy allowing "user@example.com" to perform *any*
of the actions `create`, `read`, `update`, and `delete` on a user resource,
you can use `*` one time, instead of writing four different policies to define each action individually.

```bash
{
    "action": "*",
    "resource": "auth:users:*",
    "subjects": [
        "user:local:user@example.com"
    ]
}
```

#### Considerations and Examples

This section describes some common considerations for policy wildcards.

##### A wildcard matches any value for the given term

Suppose there is a `Resource` of `cfgmgmt:nodes:*` on a policy.

Now let's say a query comes in checking permissions for node 23, so the `Resource`
in the query is `cfgmgmt:nodes:23`.
The authorization service would grant permission because the policy wildcard matches
any value in the position occupied by the wildcard.
You can see this expressed in the first row:

```text
Query                       Policy                 Allow or Deny?
-------                     ------                 --------------
cfgmgmt:nodes:23            cfgmgmt:nodes:*        Allow
cfgmgmt:nodes:509           cfgmgmt:nodes:*        Allow
cfgmgmt:nodes               cfgmgmt:*              Allow
cfgmgmt                     *                      Allow
compliance:nodes            cfgmgmt:*              Deny
compliance                  *                      Allow
```

The additional rows show the principle applies to any term of a `Resource`.

##### A wildcard includes everything deeper in the hierarchy

A policy with a `Resource` of `cfgmgmt:nodes:23:*` would match anything deeper within the same branch:

```text
Query                       Policy                 Allow or Deny?
-------                     ------                 --------------
cfgmgmt:nodes:23:runs       cfgmgmt:nodes:23:*     Allow
cfgmgmt:nodes:23:runs:199   cfgmgmt:nodes:23:*     Allow
cfgmgmt:nodes:5:runs:199    cfgmgmt:nodes:23:*     Deny
```

##### A wildcard does not include its container

A wildcard does not include the namespace in which it is contained. For example:

```text
Query                       Policy                 Allow or Deny?
-------                     ------                 --------------
cfgmgmt:nodes:23            cfgmgmt:nodes:23:*     Deny
```

Since the wildcard is contained within `cfgmgmt:nodes:23`, it cannot access that namespace;
it can only access those entities deeper within the hierarchy,
such as `cfgmgmt:nodes:23:runs` or `cfgmgmt:nodes:23:runs:199`. Another example:

```text
Query                       Policy                 Allow or Deny?
-------                     ------                 --------------
cfgmgmt:nodes:23            cfgmgmt:nodes:*        Allow
cfgmgmt:nodes               cfgmgmt:nodes:*        Deny
```

This shows a policy that grants permission for any value (node) under `cfgmgmt:nodes`.
In this scenario, the query requesting access to node 23 is granted,
but the second query to access the container of the nodes itself, is denied.

##### A literal (non-wildcard) is non-hierarchical

The absence of a wildcard in a policy means that requests must be exact matches -
there are no implied hierarchical permissions. A few examples:

```text
Query                       Policy                 Allow or Deny?
-------                     ------                 --------------
cfgmgmt:nodes               cfgmgmt:nodes          Allow
cfgmgmt:nodes:23            cfgmgmt:nodes          Deny
cfgmgmt:nodes:23            cfgmgmt:nodes:23       Allow
cfgmgmt:nodes:23:runs:99    cfgmgmt:nodes:23       Deny
```

##### Overlapping policies are harmless

You can safely add policies with resources that overlap by wildcard hierarchy inclusiveness.
In this example, each query uses the same set of policies:

```text
Query                       Policy                   Allow or Deny?
-------                     ------                   --------------
                          / cfgmgmt:nodes:*         \
cfgmgmt:nodes:23            cfgmgmt:*                Allow
                          \ cfgmgmt:nodes:23:runs:* /

                          / cfgmgmt:nodes:*         \
cfgmgmt:nodes:42            cfgmgmt:*                Allow
                          \ cfgmgmt:nodes:23:runs:* /

                          / cfgmgmt:nodes:*         \
cfgmgmt:nodes:23:runs:11    cfgmgmt:*                Allow
                          \ cfgmgmt:nodes:23:runs:* /

                          / cfgmgmt:nodes:*         \
cfgmgmt:nodes:42:runs:11    cfgmgmt:*                Allow
                          \ cfgmgmt:nodes:23:runs:* /

                          / cfgmgmt:nodes:*         \
cfgmgmt:special             cfgmgmt:*                Allow
                          \ cfgmgmt:nodes:23:runs:* /
```

## Default Authorization Settings

By default, Chef Automate is initialized with policies that allow the following:

* Admin users can perform any action on any Chef Automate resource.
* All non-Admin users can perform any action on Chef Automate resources,
  with the exception of actions on resources related to authorization (`auth`) or other system level resources.
  Only Admins can modify teams, API tokens, other users, policies, notifications, and node lifecycle settings.

See the [Default Policies]({{< relref "default-policies.md" >}}) documentation for more information
on how Chef Automate's default policies map to different functions.

## Viewing Policies

Before exercising any of the commands in the remainder of this document:

1. _(Optional)_ Install the [jq](https://stedolan.github.io/jq/) JSON processor to ensure readable output.
1. Fetch an admin API token available from the `chef-automate` CLI and set it to a usable variable.
   (See [API Tokens]({{< relref "api-tokens.md" >}}) for more information on API tokens and their use.)

    ```bash
    export TOK=`chef-automate admin-token`
    ```

1. Replace `{{< example_fqdn "automate" >}}` with your actual domain name.

### List All Existing Policies

Use the following `curl` command to list all existing policies:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
```

### List Specific Existing Policies

To view specific existing policies related to a key resource, you'll need to list all the policies
then filter for the related resource. This example filters for the `compliance` resource:

```bash
curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq '.policies[] | select(.resource | startswith("compliance"))'
```

## Restrict Permissions on Resources

To remove a user's permission to act on a resource, find and delete the policy granting it permission, including any applicable top-level policies.
This may be a policy that gives permissions directly to a user, but more likely grants access indirectly through either user teams or wildcards. If the latter, be aware that your changes will affect other users as well.

1. Find the policy (or policies) related to the permissions you'd like to remove:

    ```bash
    curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq '.policies[] | select(.resource | startswith("compliance"))'

    {
        "action": "*",
        "created_at": "2018-04-05T15:35:00.210095Z",
        "id": "cbdd1367-b731-4915-a97f-291199a63b62",
        "resource": "compliance:*",
        "subjects": [
            "*"
        ]
    }
    ```

1. Use the ID of the policy to delete it (replace the `{id}` placeholder with the actual ID):

    ```bash
    curl -s -X DELETE -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies/{id} | jq
    ```

## Common Use Cases

This section provides practical, step-by-step instructions for tailoring Chef Automate permissions to your needs.
The diagram shows the three common scenarios (permissions for all, permissions for some,
and permission for an API client); the following sections discuss each of these.

![Permissioning Use Cases](/images/docs/authz-use-cases.png)

### Open Permissions on All Resources (Default)

> As an admin, I'd like for all teams to be able to access all parts of the
application except for administrative features (creating teams, users, api keys,
policies).

With a standard installation of Chef Automate, this is automatically true thanks to
Chef Automate's [default policies]({{< relref "default-policies.md" >}}).

If you would like to allow another team to access administrative features without
adding team members to the local `admins` team, you can create a policy allowing
access to all resources with the command below.

*NOTE*: This allows a user to give authorization to any other user,
and it grants full access to all of Chef Automate's resources.

```bash
curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["user:ldap:example_user@example.com"], "action":"*", "resource":"*"}' https:/{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
```

*NOTE*: Use [chef-automate iam admin-access restore]({{< relref "cli-chef-automate.md" >}}) if you wish to restore the factory default administrator access settings after modifying the `admins` team or `Local Administrator` .

### Restrict Configuration Management Resources

> As an admin, I'd like for all teams to be able to access all parts of the application,
except for Administrative and Configuration Management features.

With a standard install of Chef Automate all users, regardless of team,
have access to Configuration Management due to two default policies:

```bash
{
    "action": "*",
    "subjects": [
    "user:*"
    ],
    "id": "4ac5f6cd-788b-4a09-bcdd-b4e2cf4395e1",
    "resource": "cfgmgmt:*",
    "effect": "allow",
    "created_at": "2018-04-10T15:00:51.696261Z"
},
{
    "action": "*",
    "subjects": [
    "user:*"
    ],
    "id": "cbdd1367-b731-4915-a97f-291199a63b62",
    "resource": "cfgmgmt",
    "effect": "allow",
    "created_at": "2018-04-10T15:00:51.696261Z"
}
```

If you're wondering what API endpoints those default policies apply to specifically,
see the [Default Policy]({{< relref "default-policies.md" >}}) documentation for details.

To remove that access, delete policies permitting all users to access `cfgmgmt` or `cfgmgmt:*`:

1. Fetch `cfgmgmt` policies:

    ```bash
    curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq '.policies[] | select(.resource | startswith("cfgmgmt"))'
    ```

1. Delete those policies using their IDs, one at a time (replace the `{id}` placeholder with the actual ID):

    ```bash
    curl -s -X DELETE -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies/{id} | jq
    ```

   Now only users with admin-level permissions have access to anything related
to Configuration Management. You can follow the same sequence of steps to delete permissions
to Compliance resources under the `compliance` default policies.

> As an admin, I'd like for all users to be able to perform any action on Compliance,
and for some users to be able to perform any action on Configuration Management (or vice versa).

To accomplish this, you'll need to follow the steps described in the section directly above.
Once you've deleted the default Configuration Management policies,
here's how you would give that permission to a specific user or team:

1. Have your list of teams you'd like to grant access.
1. Create a new policy permitting the specified teams/users to access Configuration Management:

    ```bash
    curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["team:saml:ops"], "action":"*", "resource":"cfgmgmt:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
    ```

   Now only members of the `ops` team may access Configuration Management data and functionality.
You can follow the same sequence of steps to delete the default Compliance policy
and replace it with a new policy restricted to specific teams.

> As an admin, I'd like for some teams to be able to perform any action on Compliance resources,
and for other teams to be able to perform any action on Configuration Management resources.

In this situation, you'll want to delete the default policies for both Configuration Management and Compliance:

1. Fetch `cfgmgmt` and `compliance` policies.

    ```bash
    curl -s -H "api-token: $TOK" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq '.policies[] | select(.resource | startswith("cfgmgmt\|compliance"))'
    ```

1. Delete each of those policies using their IDs (replace the `{id}` placeholder with the actual ID):

    ```bash
    curl -s -X DELETE -H "api-token: $TOK" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies/{id} | jq
    ```

1. Create policies permitting the specified teams/users to access `cfgmgmt` and `compliance` as necessary:

    ```bash
    curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["team:ldap:ops"], "action":"*", "resource":"cfgmgmt:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq

    curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["team:ldap:sec"], "action":"*", "resource":"compliance:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
    ```

In this example, only members of the `ops` LDAP team can access Configuration Management data
and functionality, while members of the `sec` LDAP team can access Compliance data and functionality.

### Permission for an API Client

> As an admin, I would like to create a token that gives a client permission to read any Compliance resource.

1. Create an API token.

    ```bash
    curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"description":"My compliance token"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/tokens | jq .id

    95aef20b-0a4e-4698-bd69-ce2cf44c2e35
    ```

1. Copy the ID that is returned.

1. Create policies to permit that client to read `compliance:*`

    ```bash
    curl -s -H "api-token: $TOK" -H "Content-Type: application/json" -d '{"subjects":["token:95aef20b-0a4e-4698-bd69-ce2cf44c2e35"], "action":"read", "resource":"compliance:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq
    ```
