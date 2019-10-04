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

This API reference details Chef Automate IAM v2 features from the command line.
If you are not already on IAM v2, see the [IAM v2 User's Guide]({{< relref "iam-v2-guide.md#upgrade-to-iam-v2" >}}) to upgrade.
Any command with a browser equivalent includes brief directions for locating it on the corresponding page.
See the [IAM v2 Overview]({{< relref "iam-v2-overview.md" >}}) for an overview and exposition of new concepts.
See the [IAM v2 User's Guide]({{< relref "iam-v2-guide.md" >}}) for more information on setting up and using IAM v2 features.

## General Notes

1. If you do not have one already, generate an admin token for IAM v2.
   (Neither The v1 command `chef-automate admin-token` nor any v1 admin tokens will work for v2.)
   Use this, filling in a token name of your choice:

   ```bash
   export TOKEN=`chef-automate iam token create <your-token-name-here> --admin`
   ```

2. URIs are relative to `https://<your-domain-here>/apis/iam/v2beta`, unless otherwise noted.

3. Attach the `?pretty` query string to an endpoint to get pretty-printed output.

Putting it all together with the `/policies` endpoint as an example, this command fetches the list of policies with multi-line, formatted output:

```bash
curl -sH "api-token: $TOKEN" \
  https://{{< example_fqdn "automate" >}}/apis/iam/v2beta/policies?pretty
```

For those API methods that take JSON data (typically `create` and `update` methods)
you can provide that data to the REST endpoint in a variety of ways.
If it is a relatively small size payload, you can include it in a `curl` command inline, e.g.

```bash
curl -sH "api-token: $TOKEN" -d '<your JSON here>' ...
```

If the payload is larger, it is often convenient to store it in a file, then pass that file, e.g.,

```bash
curl -sH "api-token: $TOKEN" -d @policy.json ...
```

## Notes on the Projects Property

Teams, tokens, roles, and policies all contain a top-level `projects` property; you will see that in the example JSON for each resource in the following sections.
This property associates the particular IAM resource with one or more projects.
Users must have permissions for those projects to be able to view or modify those resources.
This `projects` property may be empty, which would give access only to users included on policies specifying the special `(unassigned)` project designation.
This `projects` property may **not** specify `*` to indicate all projects.

To create those permissions for users, there is a separate and distinct `projects` property on each policy statement.
This `projects` property may **not** be empty; you must either specify specific projects or use a wildcard (`*`) to indicate all projects.

Here is a summary:

Type               | May be empty ? | Include * ? | May specify (unassigned) ?
-------------------|----------------|-------------|------------
top-level projects |    yes         |   no        |  no
statement projects |    no          |   yes       |  yes

## Policies

Method | HTTP request              | Description
-------|---------------------------|------------
list   | GET /policies             | lists both *Chef-managed* and *Custom* policies
get    | GET /policies/***id***    | gets the specified policy
create | POST /policies            | creates a *Custom* policy
update | PUT /policies/***id***    | updates a *Custom* policy
delete | DELETE /policies/***id*** | deletes a *Custom* policy

**Example Policy:**

This policy shows two statements, each enforcing a different set of permissions.
The first policy statement grants list access to view users.
The second statement grants access to all the actions comprising the `editor` role.
Note that you may specify actions either inline with **actions** or using a **role** reference.
(Within a single statement it is even OK to use both if you like; the set of actions is the union of the inline actions and the actions defined by the role.)

```json
{
  "name": "Test Policy",
  "id": "test-policy-1",
  "members": ["team:local:alpha","team:ldap:beta"],
  "statements": [
    {
      "effect": "ALLOW",
      "actions": [
        "iam:users:list",
        "iam:users:get"
      ],
      "projects": [
        "*"
      ],
    },
    {
      "effect": "ALLOW",
      "role": "editor"
    }
  ],
  "projects": [
    "admin-group-1",
    "it-support",
  ]
}
```

### Listing Policies

The output includes both *Chef-managed* and *Custom* policies.
Policies migrated from IAM v1 are included as *Custom* policies.
Policy names that begin with `[Legacy]` are default policies migrated from v1.
We recommend deleting these legacy policies and setting up new IAM v2 policies in their place.

*In the browser:* **Settings**  >> **Policies**

### Getting a Policy

In order to fetch a policy on the command-line, you must know its ID.
This may be obtained by first fetching the list of all policies and getting the `id` field of the policy of interest.
For example, the ID for the default administrator policy is `administrator-access`.

*In the browser:* **Settings**  >> **Policies** >> [select a policy]

### Creating a Policy

Create a policy by composing JSON with all the necessary policy properties (see the beginning of the section above).

### Updating a Policy

* You can modify the *membership* of both Chef-managed and custom policies; see [Policy Membership]({{< relref "iam-v2-api-reference.md#policy-membership" >}}) for more information.
* You can modify the *definition* of custom policies but not Chef-managed policies.
* The policy ID is immutable; it can only be set at creation time.
* The update operation modifies *all* policy properties, not just the ones you specify.
  Properties that you do not specify are reset to empty values.
  Thus, you should supply all of a policy's properties, not just the ones you wish to update.

### Deleting a Policy

Deleting a policy is permanent and cannot be undone.

*In the browser:* **Settings**  >> **Policies** >> [open control menu for target policy] >> Delete

## Policy Membership

Chef Automate supports two methods for updating policy membership.

Using `PUT` lets you set membership for the entire policy; see [Setting Policy Membership]({{< relref "iam-v2-api-reference.md#setting-policy-membership" >}}).

Using `POST` lets you add members to (or remove members from) an existing membership list.
For more information, see [Adding Members to Policies]({{< relref "iam-v2-api-reference.md#adding-members-to-policies" >}}) and [Removing Members from Policies]({{< relref "iam-v2-api-reference.md#removing-members-from-policies" >}}).

Method | HTTP request                           | Description
-------|----------------------------------------|------------
list   | GET /policies/***id***/members         | lists the membership
update | PUT /policies/***id***/members         | replaces the membership
add    | POST /policies/***id***/members:add    | add to the membership
remove | POST /policies/***id***/members:remove | remove from the membership

**Example payload:**

```json
{
  "members": [
    "team:local:admins",
    "token:admin-token"
  ]
}
```

### Listing Policy Members

The output lists all defined members of the specified policy.

### Updating All Members on a Policy

Specifying a complete list of members is often simpler than keeping track of a policy's membership and adding or deleting specific members (whether users, team, or tokens).
Use this HTTP request to replace the policy's membership with a full, new list.

### Adding Members to a Policy

Specify just new members to add to a policy's membership via this HTTP request.

*In the browser:* **Settings**  >> **Policies** >> [select a policy] >> **Members** >> **Add Members**

Select any local users or teams listed to add to the policy, or use **Add Member Expressions** for tokens, or LDAP or SAML users or teams.

### Removing Members from a Policy

Specify just members to remove from a policy's membership via this HTTP request.
The removed members still exists within Chef Automate, but are no longer associated with this policy.

*In the browser:* **Settings**  >> **Policies** >> [select a policy] >> **Members** >> [open control menu for target member] >> Remove Member

## Roles

A role encapsulates a list of actions.
IAM v2 provides several default *Chef-managed* roles, which are essential to the operation of Chef Automate and cannot be altered.
Roles you create are *Custom* roles.

Method | HTTP request              | Description
-------|---------------------------|------------
list   | GET /roles                | lists both *Chef-managed* and *Custom* roles
get    | GET /roles/***id***       | gets the specified role
create | POST /roles               | creates a *Custom* role
update | PUT /roles/***id***       | updates a *Custom* role
delete | DELETE /roles/***id***    | deletes a *Custom* role

**Default Chef-managed Roles:**

Chef-managed Role Name | ID| Actions
-----------------------|-----|--------
Owner              | owner         | \*
Viewer             | viewer        | secrets:\*:get, secrets:\*:list, infra:\*:get, infra:\*:list, compliance:\*:get, compliance:\*:list, system:\*:get, system:\*:list, event:\*:get, event:\*:list, ingest:\*:get, ingest:\*:list, iam:projects:list, iam:projects:get, applications:\*:list, applications:\*:get
Editor             | editor        | infra:\*, compliance:\*, system:\*, event:\*, ingest:\*, secrets:\*, telemetry:\*, iam:projects:list, iam:projects:get, iam:projects:assign, applications:\*
Project Owner      | project-owner | infra:\*, compliance:\*, system:\*, event:\*, ingest:\*, secrets:\*, telemetry:\*, iam:projects:list, iam:projects:get, iam:projects:assign, iam:policies:list, iam:policies:get, iam:policyMembers:\*, iam:teams:list, iam:teams:get, iam:teamUsers:\*, iam:users:get, iam:users:list
Ingest             | ingest        | infra:ingest:\*, compliance:profiles:get, compliance:profiles:list

**Example Role:**

```json
{
  "name": "Test Role",
  "id": "test-role-1",
  "actions": [
    "infra:*",
    "compliance:*",
    "teams:*",
    "users:*"
  ],
  "projects": [
    "east-region",
    "west-region"
  ]
}
```

### Listing Roles

The output includes both *Chef-managed* and *Custom* roles.

*In the browser:* **Settings**  >> **Roles**

### Getting a Role

This shows the details of a single role, selected by its ID.

*In the browser:* **Settings**  >> **Roles** >> [select a role]

### Creating a Role

Create a role by composing JSON with all the necessary role properties (see the beginning of the section above).

### Updating a Role

The update operation modifies *all* role properties, not just the ones you specify.
Properties that you do not specify are reset to empty values.
Thus, you should supply all of a role's properties, not just the ones you wish to update.
Note that the ID is immutable; it can only be set at creation time.

### Deleting a Role

Deleting a role is permanent and cannot be undone.

*In the browser:* **Settings**  >> **Roles** >> [open control menu for target role] >> Delete

## Tokens

Method | HTTP request              | Description
-------|---------------------------|------------
list   | GET /tokens               | lists all tokens
get    | GET /tokens/***id***      | gets the specified token
create | POST /tokens              | creates a *Custom* token
update | PUT /tokens/***id***      | updates a *Custom* token
delete | DELETE /tokens/***id***   | deletes a *Custom* token

**Example Token:**

```json
{
  "name": "token 1",
  "id": "token-1",
  "active": true,
  "projects": [
    "east-region",
    "west-region"
  ]
}
```

### Listing Tokens

The lists admin and non-admin tokens.

### Getting a Token

This shows the details of a single token, selected by its ID.

### Creating a Token

Create a token by composing JSON with all the necessary token properties (see the beginning of the section above).
Note that this creates **non**-admin tokens that may then be assigned permissions via policies just like users or teams (unless you have already created policies that encompass all tokens using `tokens:*`).

You cannot create admin tokens via the REST API.
Admin tokens can only be created by specifying the `--admin` flag to this `chef-automate` sub-command:

```bash
  chef-automate iam token create <your-token-name> --admin
```

### Updating a Token

The update operation modifies *all* token properties, not just the ones you specify.
Properties that you do not specify are reset to empty values.
Thus, you should supply all of a token's properties, not just the ones you wish to update.
Note that the ID is immutable; it can only be set at creation time.

### Deleting a Token

Deleting a token is permanent and cannot be undone.

*In the browser:* **Settings**  >> **API Tokens** >> [open control menu for target token] >> Delete Token

## Projects

Method | HTTP request              | Description
-------|---------------------------|------------
list   | GET /projects             | lists all projects
get    | GET /projects/***id***    | gets the specified role
create | POST /projects            | creates a project
update | PUT /projects/***id***    | updates a project
delete | DELETE /projects/***id*** | deletes a project

**Example Project:**

```json
{
  "name": "Test Project",
  "id": "test-project-1"
}
```

### Listing Projects

The output lists all defined projects.

*In the browser:* **Settings**  >> **Projects**

### Getting a Project

This shows the details of a single project, selected by its ID.

*In the browser:* **Settings**  >> **Projects** >> [select a project]

### Creating a Project

Create a project by composing JSON with all the necessary project properties (see the beginning of the section above).

### Updating a Project

A project has only one modifiable property, the project name.
The ID is immutable; it can only be set at creation time.

### Deleting a Project

Deleting a project is permanent and cannot be undone.

*In the browser:* **Settings**  >> **Projects** >> [open control menu for target project] >> Delete Project

## Project Rules

Method | HTTP request                                          | Description
-------|-------------------------------------------------------|------------
list   | GET /projects/***project_id***/rules                  | lists rules for a project
get    | GET /projects/***project_id***/rules/***rule_id***    | gets a project rule
create | POST /projects/***project_id***/rules                 | creates a project rule
update | PUT /projects/***project_id***/rules/***rule_id***    | updates a project rule
delete | DELETE /projects/***project_id***/rules/***rule_id*** | deletes a project rule

A rule specifies a non-empty array of conditions.
Each rule **condition** specifies values to match for a particular **attribute**.
The choice of attributes depends on the rule **type**.

Rule Type  | Available Attributes
-----------|---------------------
EVENT      | CHEF_ORGANIZATION, CHEF_SERVER
NODE       | CHEF_ORGANIZATION, CHEF_SERVER, ENVIRONMENT, CHEF_ROLE, CHEF_TAG, CHEF_POLICY_NAME, CHEF_POLICY_GROUP

Each rule condition also specifies an **operator**, either `MEMBER_OF` or `EQUALS`.
To match a single value, use `EQUALS` and include just a single value in the **values** array.
To match any one of a set of values, use `MEMBER_OF` and include the choices in the **values** array.

**Example Rule:**

```json
{
  "id": "test-rule",
  "name": "my test rule",
  "type": "NODE",
  "project_id": "eastern_region",
  "conditions": [
    {
      "operator": "MEMBER_OF",
      "attribute": "CHEF_SERVERS",
      "values": [
        "prod",
        "staging"
      ]
    },
    {
      "operator": "EQUALS",
      "attribute": "CHEF_ORGANIZATION",
      "values": [
        "org1"
      ]
    }
  ]
}
```

### Listing Rules for a Project

The output lists all rules for a specified project.

*In the browser:* **Settings**  >> **Projects** >> [select a project]

### Getting a Project Rule

This shows the details of a single project, selected by its ID.

*In the browser:* **Settings**  >> **Projects** >> [select a project] >> [select a rule]

### Creating a Project Rule

Create a rule by composing JSON with all the necessary rule properties (see the beginning of the section above).

### Updating a Project Rule

The update operation modifies *all* rule properties, not just the ones you specify.
Properties that you do not specify are reset to empty values.
Thus, you should supply all of a rule's properties, not just the ones you wish to update.
Note that the ID is immutable; it can only be set at creation time.

### Deleting a Project Rule

Deleting a rule is permanent and cannot be undone.

*In the browser:* **Settings**  >> **Projects** >> [select a project] >> [open control menu for target rule] >> Delete Rule

## Restoring Admin Access

While we have [safeguards]({{< relref "iam-v2-overview.md#policy-types" >}}) to prevent it, it is possible to lock yourself out of Chef Automate.
If you have root access to the node where Chef Automate is installed, use the following commands to restore admin access:

<!-- Wording/Replace password/replace token/v1 commands won't work on c2 -->

* This command, which is also available on IAM v1, resets the local `admin` user's password and ensures that user is a member of the local `admins` team, which is a permanent member of the Chef-managed `Administrator` policy.

```bash
  chef-automate iam admin-access restore <your new password here>
```

* Generate a new token and add that token as a new member of the Chef-managed `Administrator` policy. This is the equivalent of the v1 command `chef-automate admin-token`.

```bash
  chef-automate iam token create <your token name here> --admin
```
