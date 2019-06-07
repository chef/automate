+++
title = "IAM v2 Overview"
description = "IAM v2 on Chef Automate"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "beta"
    weight = 10
+++
<!-- Goal
Show the simplest possible experience that most users will experience. We are assuming that experience will be UI only, so present the UI information first followed by CLI information presented in reference format.
-->

This guide helps you understand and use the beta version of Chef Automate's new **Identity and Access Management** ([IAM](https://en.wikipedia.org/wiki/Identity_management)) system (IAM v2).

IAM v2 is a beta release and its functionality is subject to change during this period, based on customer feedback.
This is an opt-in only feature during the beta period.
Chef Automate users will not be automatically upgraded.

We designed IAM v2 to leave your v1 policy data untouched during your upgrade to v2. We do, however, recommend that you do not
migrate v1 policies upon upgrade as they can interfere with expected behavior; to that end, we have provided a
`--skip-policy-migration` flag that may be used with the upgrade command.
Whether or not you migrate your v1 policies, if at any time you decide to opt back out of the beta and revert to v1, your original v1 policies will still be intact.
Reverting to v1, however, will remove any new v2 policies or roles created while using IAM v2.
Note that this applies only to policies and roles in this beta period. Users, teams, and tokens are currently shared between v1 and v2, but these are slated for separation before the full v2 release.

See the [IAM v2 API Reference]({{< relref "iam-v2-api-reference.md" >}}) for day-to-day use of the IAM v2 system.

## New Features in IAM v2

IAM v2 expands Chef Automate's authorization system by supporting policies to allow multiple permissions, separating out policy membership from policy definition for more fine-grained control, and adding roles to begin moving towards role-based access control.
Additionally, IAM v2 improves the user experience by starting to expose policy management in the Automate user interface.

At the heart of Chef Automate's IAM system is the *policy*.
A policy defines permissions for who may perform what action on which resource.
The "who" may be a user, a team, or a system.
Users and teams are designated by name while systems use pre-authorized tokens to communicate with Automate.

Conceptually, this description applies equally to both v1 and v2.
However, for IAM v2, we have revised and expanded what constitutes a policy.
The following diagram shows the new policy structure; we'll detail the specifics in the next few sections.

![](/images/docs/iam-v2-diagram.png)

### New Policy Definition

IAM v2 introduces multi-statement policies, so more complex permissions can be embodied in a single policy, resulting in fewer policies required to secure your system.
Each statement specifies a single permission.
The net effect (ALLOW or DENY) of a policy is determined by evaluating the effect of each statement and combining them: if there is at least one statement allowing access, and none denying it, then the policy allows access. Otherwise, it is denied.

Taking the aggregate of all policies, access is granted only if explicitly allowed in any policy, and not explicitly denied in any policy.
IAM policy evaluation order is:

1. By default, all requests are denied.
2. Any explicit ALLOW overrides the default DENY.
3. Any explicit DENY in any policy overrides any ALLOWs in any policy.

Examples:

- Neither user1, nor any team that user1 belongs to, are included in any policy with the action `iam:users:list`.
  Net effect: user1 is denied, and will not be able to view the list of users.
  (This illustrates the global default DENY condition.)
- Bob is a member of team alpha and team omega.
  Team alpha has a broad range of permissions while team omega is very restricted in what its members may access.
  The "major teams" policy has a statement that allows team alpha to access the resource `compliance:reporting:nodes` while it has another statement that denies team omega access to the that very same resource.
  Net effect: Bob is denied access because there is at least one statement denying access.
- Mary is a member of the default viewers team, which allows read access to everything because
  of the default Viewers policy.
  Mary is also a member of the deployment team, which has permission to upload and download profiles.
  Net effect: Mary is granted read access to everything and Mary can also upload/download profiles,
  because neither of those policies explicitly denies something allowed by the other.

#### Properties of a Policy

A v2 policy consists of a list of **statements**, where each statement describes a permission. This is a departure from v1 policies, which had _action_, _resource_, and _effect_ as top-level properties.

Property   | Description
-----------|------------
Statements | list of permissions

#### Properties of a Policy Statement

A policy statement must include one or more *actions*, which can be defined *inline* with the `Actions` property or by reference through a role definition, specified in the `Role` property.
Statements are typically composed using roles, but inline policy statements are useful for managing access to specific resources for special cases, or for when you don't want to create a named role for monolithic policies.

Property   | Description
-----------|-----------------------------
Actions    | list of operations, e.g., read IAM users, get compliance profiles, update node status, etc.
Role       | a named set of actions
Effect     | ALLOW or DENY

#### Properties of a Role

A v2 role consists of a list of **actions**.
Roles are discussed in detail in [Basic Role-Based Access Control]({{< relref "iam-v2-api-reference.md#roles" >}}).

Property   | Description
-----------|-----------------------------
Actions    | list of operations the role manages, e.g., read IAM users, get compliance profiles, update node status, etc.

### Members and Policies

A **member**  (called a _subject_ in v1) may be a user, a team, or a token.
Users and teams may be *local*, meaning they are defined within Chef Automate, or managed by an external identity provider, specifically LDAP or SAML.
In this so far, v2 and v1 behave the same.

What is new in v2 is that policy *membership* is separate and distinct from policy *definition*.
(Notice that members were **not** included as part of the [New Policy Definition]({{< relref "iam-v2-overview.md#new-policy-definition" >}})).
Notably, with IAM v2, you can modify policy membership for any policy,
but you can only adjust policy definition for policies that you create yourself.
[Policy Types]({{< relref "iam-v2-overview.md#policy-types" >}}) discusses this point further.

### Policy Types

IAM v2 distinguishes two types of policies: *Chef-managed* and *custom*.
*Chef-managed* policies are provided by Chef and are integral to the operation of Chef Automate. The policy statements (comprising the policy definition) in Chef-managed policies are immutable.
*Custom* policies are those you create for your own needs. You can add, edit, and delete policy statements in your custom policies.

You can modify the membership of either type of policy.
It is, however, not possible to remove the local Administrator from the Administrator policy.

### Basic Role-Based Access Control

IAM v2 also introduces basic roles as our first step to delivering
Role-Based Access Control ([RBAC](https://en.wikipedia.org/wiki/Role-based_access_control)).
A role is a named set of actions.
This provides the benefits of *encapsulation* (only needing to know the name and not be encumbered by all the details after initial definition) and *reuse* (apply the role to any statement that needs it).
For example, one of the default roles provided is `Viewer`.
That role is comprised of a number of actions.
You could enumerate those actions in the `Actions` property of a given statement, or you can specify the single role name `Viewer` in the `Role` property of the statement; the result is identical either way.

Chef Automate ships with the default roles shown below.
The descriptions shown should typically be all the detail you need, but for the actions comprising the roles see [Chef-managed Roles]({{< relref "iam-v2-api-reference.md#roles" >}}) or inspect the roles in your browser.

Role        | Description
------------|------------
Viewer      | can **view** everything in the system *except* IAM
Editor      | can **do** everything in the system *except* IAM
Owner       | can **do** everything in the system *including* IAM
Ingest      | can ingest data into the system and get resources needed to generate data e.g., profiles (mainly used for tokens)

Just as policies can be *Chef-managed* or *custom*, so too, can roles.
These default roles are Chef-managed, so they, like Chef-managed policies, are immutable.

You can use these default roles in your own policies, or create your own roles for more customized permissions.
Any roles you create can be modified later if you so choose.

Chef Automate also ships with several default policies that leverage these default roles.
The default policies are: Viewers, Editors, Administrator, and Ingest.

Just add members to any of these default policies, as described later, to quickly get your basic users up and running.

### Policies and Roles in Automate

<!-- I do not like saying "user interface" because technically that includes the command line, too. But I can live with it if you think that is the lesser of two evils. -->
IAM v2 introduces partial policy management from within Chef Automate in your browser.

![](/images/docs/admin-policies.png)

Chef Automate's **Settings** tab has a new _Access Management_ heading on the left panel, with new pages for both _Policies_ and _Roles_.
Here, you'll see all of the policies and roles listed out on their respective pages, and an indication of their types, Chef-managed or custom.

![](/images/docs/admin-policies-administrator-access.png)

Select a policy or role to open its definition.
For policies, use the **Members** tab to manage the membership of the policy, right from the browser.

![](/images/docs/admin-roles-chef-managed-owner-role.png)

Though membership covers users, teams, and tokens, there is a useful feature specific to just local users and teams.
Because local users and teams are managed by Chef Automate directly (as opposed to LDAP and SAML users and teams), Automate provides a convenient mechanism for streamlining the process.
When you open the _Add Members_ page, Automate lists all your local members but filters out those already attached to the policy, giving you a smaller list to peruse.
