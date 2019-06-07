+++
title = "IAM v2 Users Guide"
description = "Try out IAM v2 on Chef Automate"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "beta"
    weight = 20
+++

This guide shows you how to upgrade Chef Automate to IAM v2, perform important administrative operations, and revert back to IAM v1.
After upgrading to IAM v2, you'll add members to Chef-managed v2 policies, delete a legacy policy, and write a Team Admin v2 policy that lets a Team Admin manage their users and teams.

## Upgrade to IAM v2

Perform the upgrade to IAM v2 with `chef-automate iam upgrade-to-v2`; the response should look something like:

```terminal
Migrating v1 policies...

Updating deployment configuration

Applying deployment configuration
  Started automate-gateway

Creating default teams Editors and Viewers...

Success: Enabled IAM v2
```

Note: We recommend users do not migrate existing IAM v1 policies as they can interfere with expected behavior. To upgrade
without porting existing policies, use the `--skip-policy-migration` flag: `chef-automate iam upgrade-to-v2 --skip-policy-migration`.

## View Policies

After you've logged in to Chef Automate, select the **Settings** tab in the top navigation bar, then select and locate the `Policies` section on the left hand panel.

This lists all of your v2 policies:

* New default (Chef-managed) policies: Administrator, Ingest, Editors, and Viewers.
* Imported v1 default policies--now called *legacy policies*--in the new v2 policy format and marked with the `[Legacy]` prefix.
* Imported v1 custom policies that you created; these are marked with the `[Legacy]` prefix and a `(custom)` suffix.

![](/images/docs/admin-policies-migrated.png)

## Policy Conversion

Now that you are up-and-running with v2 policies, as the first step we recommend that you reconstitute your v1 policies as v2 policies.
Once that is done, then delete the old legacy v1 policies and you will have a clean, up-to-date system.

The next few sections explain how to use Chef-managed policies and how to create custom policies.

### Using Chef-Managed Policies

In this example conversion, you will create two local users and add them to default teams that are automatically included in default policies.
Note that you could also add users directly to policies without the intermediate teams, but using teams make managing your system more flexible.

#### Create Users

Follow [Creating Users]({{< relref "users.md#creating-users" >}}) to:

* Create a local user with the username `test_viewer`.
* Create a local user with the username `test_editor`.

#### Add Users to Teams

Open the `Team` section of Chef Automate by selecting `Teams` from the left hand panel of the **Settings** tab.
Three teams are provided by default: `admins`, `viewers`, and `editors`.

Follow [Adding Users to a Team]({{< relref "teams.md#adding-users-to-a-team" >}}) to:

* Add the user `test_viewer` to the Chef-managed `viewers` team.
* Add the user `test_editor` to the Chef-managed `editors` team.

Out-of-the-box, those teams are part of the `Viewers` and `Editors` policies, respectively.
The default `Viewers` policy is provided so you can quickly grant read-only access to everything in Chef Automate except for admin resources.
Similarly, the default `Editors` policy is provided so you can quickly grant complete access to everything in Chef Automate except for admin resources.
Once this step is done the `test_viewer` and `test_editor` users may log in with appropriate system access.

### Creating Custom Policies

The Chef-managed default policies give you a starting-point for permissions.
You will likely want to write more fine-grained policies, tailored to the demands of your organization.
Defining your own policies is a straightforward process; however, it must be done from the command line.

As an example, let's say you, the admin, want to delegate a portion of your tasks to a colleague, but without granting them full admin access.
In this case, you could create a policy called `Team Managers`, which grants its members some--but not all--administrative privileges.
Create a JSON file in the editor of your choice.
Following JSON syntax, begin with the name property.
(For this example's complete JSON, see the end of this section.)

```json
  "name": "Team Managers",
```

That name is for human consumption; when you want to refer to the policy in commands you will need to know its ID.
So let's give this policy the ID `team-managers-admin`.

```json
  "id": "team-managers-admin",
```

Let's further assume you want user `Bob` as well as anyone on team `gamma` to be authorized by this policy; this comprises the `members` array.

```json
  "members": [
    "user:local:bob",
    "team:local:gamma",
  ],
```

Next, you'll specify the permissions themselves, which in IAM v2 are the `statements`, declared as an array.
First, write a statement that **allows** access to the _get_, _list_, and _update_ actions for _users_ and _teams_:

```json
    {
      "effect": "ALLOW",
      "actions": [
        "iam:users:update",
        "iam:users:list",
        "iam:users:get",
        "iam:teams:update",
        "iam:teams:list",
        "iam:teams:get"
      ]
    },
```

Next, write a statement that **denies** access to the _create_ and _delete_ actions:

```json
    {
      "effect": "DENY",
      "actions": [
        "iam:users:create",
        "iam:teams:create",
        "iam:users:delete",
        "iam:teams:delete"
      ]
    }
```

The complete policy should look like:

```json
{
  "name": "Team Managers",
  "id": "team-managers-admin",
  "members": [
    "user:local:bob",
    "team:local:gamma"
  ],
  "statements": [
    {
      "effect": "ALLOW",
      "actions": [
        "iam:users:update",
        "iam:users:list",
        "iam:users:get",
        "iam:teams:update",
        "iam:teams:list",
        "iam:teams:get"
      ]
    },
    {
      "effect": "DENY",
      "actions": [
        "iam:users:create",
        "iam:teams:create",
        "iam:users:delete",
        "iam:teams:delete"
      ]
    }
  ]
}
```

Save your JSON file and follow the steps in [Creating a Policy]({{< relref "iam-v2-api-reference.md#creating-a-policy" >}}) to pass that data to Chef Automate.

### Policy Membership

Users, teams, and tokens can all be policy members, and both users and teams can be either locally or externally managed (LDAP or SAML).

#### Local Users and Teams

Local users are users and teams managed directly by Chef Automate.
Chef Automate has knowledge of your local members and can streamline their management.

To add or delete members, navigate to the Policies list in the **Settings** tab, and then select a policy in the list to open its details.
Select **Members** to view the current membership.
Use the **Add Members** button to open a list of candidate members.
This lists all those local members (both users and teams) that are *not* members of this policy.
If all of the local members are already included in the policy, then this list will be empty.
Select any members you wish to add to the policy.
Use the **Add # Members** button to complete the operation.
This takes you back to the policy details, showing the revised membership list.

#### Member Expressions

Member expressions are required for externally managed users and for tokens.

LDAP and SAML users' information is saved outside of Chef Automate so, unlike local users and teams, Automate cannot provide a list for you to pick from.
Instead, you'll need to manually enter the provider-qualified user or team.
To do this, open any policy from the _Policies_ list, then select **Members**.
Select **Add Members** to open the list of candidate local users and teams.
Near the bottom of the page select the **Add Member Expression** button.

Enter a member expression using the format `team:<type>:<name>` or `user:<type>:<name>`; note these are case-sensitive.

* The `<type>` expression is either `ldap` or `saml` according to whichever [external identity provider]({{< relref "configuration.md#authentication-via-existing-identity-management-systems" >}}) Chef Automate has been configured to use.
* The `<name>` expression is the name of the user or team that the external identity provider knows. For example, this is a valid member expression `team:ldap:editors_team_1`, assuming the `editors_team_1` team is known by your identity provider.

Alternately, you may add *all* teams to a policy using a wildcard as the last term in the member expression: `team:ldap:*` or `team:saml:*`.

The member expression dialog also supports tokens, because candidate tokens are not listed for you either.
You enter a token using the expression `token:<id>`.
In order to find a token's ID, use the command in the API reference for [listing tokens]({{< relref "iam-v2-api-reference#listing-tokens" >}}).

## Removing Legacy Policies

Once you've rewritten your v1 policies as v2 policies, you should remove the v1 legacy policies.
This section walks you through deleting all of the legacy policies imported into v2, which will provide you a clean slate for exploring the IAM v2 Beta.

Open the menu on any custom policy, located at the end of the policy row, and select **Delete Policy** from the menu.
(You will not have this option for Chef-managed policies.)
A warning appears if members are still attached to the policy, because deleting that policy disrupts access for all of its members.
However, you can still delete the policy.
Repeat for each legacy policy.

## Reverting to IAM v1

Should you have a need to revert back to IAM v1, it is quick and easy to do:

```console
$ chef-automate iam reset-to-v1
```

Note that this discards your IAM v2 policies and roles, and re-configures Chef Automate to use your v1 policies again.
