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

This guide helps you understand and use the beta version of Chef Automate's new **Identity and Access Management** ([IAM](https://en.wikipedia.org/wiki/Identity_management)) system (IAM v2.1).

IAM v2.1 is a beta release and its functionality is subject to change during this period, based on customer feedback.
This is an opt-in only feature during the beta period.
Chef Automate users will not be automatically upgraded.

We designed IAM v2.1 to leave your v1 policy data untouched during your upgrade to v2.1; however, you can choose to not port over v1 policies by using the provided `--skip-policy-migration` flag with the upgrade command.

Whether or not you migrate your v1 policies, if at any time you decide to opt back out of the beta and revert to v1, your original v1 policies will still be intact.
Reverting to v1, however, will remove any new v2.1 data created while using IAM v2.1 (policies, roles, projects, rules).
Users, teams, and tokens are shared between v1 and v2.1 so changes to them will persist.

See the [IAM v2 API Reference]({{< relref "iam-v2-api-reference.md" >}}) for day-to-day use of the IAM v2 system.

## New Features in IAM v2.1

IAM v2.1 expands Chef Automate's authorization system by supporting policies to allow multiple permissions, separating out policy membership from policy definition for more fine-grained control, and adding roles to begin moving towards role-based access control.
Additionally, IAM v2.1 improves the user experience by starting to expose policy management in the Automate UI.

At the heart of Chef Automate's IAM system is the *policy*.
A policy defines permissions for who may perform what action on which resource.
The "who" may be a user, a team, or a system.
Users and teams are designated by name while systems use pre-authorized tokens to communicate with Automate.

Conceptually, this description applies equally to both v1 and v2.1.
However, for IAM v2.1, we have revised and expanded what constitutes a policy.
The following diagram shows the new policy structure; we'll detail the specifics in the next few sections.

![](/images/docs/iam-v2-diagram.png)

## New Policy Definition

IAM v2.1 introduces multi-statement policies, so more complex permissions can be embodied in a single policy, resulting in fewer policies required to secure your system.
Each statement specifies a single permission.
The net effect (ALLOW or DENY) of a policy is determined by evaluating the effect of each statement and combining them: if there is at least one statement allowing access, and none denying it, then the policy allows access. Otherwise, it is denied.

Considering the aggregate of all policies, access is granted only if explicitly allowed in any policy, and not explicitly denied in any policy.
IAM policy evaluation order is:

1. By default, all requests are denied.
2. Any explicit ALLOW overrides the default DENY.
3. Any explicit DENY in any policy overrides any ALLOWs in any policy.

Examples:

- Neither user1, nor any team that user1 belongs to, are included in any policy with the action `iam:users:list`.
  Net effect: user1 is denied access to view the list of users.
  (This illustrates the global default DENY condition.)
- Bob is a member of team alpha and team omega.
  Team alpha has a broad range of permissions while team omega is very restricted in what its members may access.
  The "major teams" policy has a statement that allows team alpha to access the resource `compliance:reporting:nodes` while it has another statement that denies team omega access to that very same resource.
  Net effect: Bob is denied access because there is at least one statement denying access.
- Mary is a member of the default viewers team, which allows read access to everything because of the default Viewers policy.
  Mary is also a member of the deployment team, which has permission to upload and download profiles.
  Net effect: Mary is granted both those permissions (effectively the union of relevant policies).

### Properties of a Policy

A v2.1 policy consists of a list of **statements**, where each statement describes a permission.

Property   | Description
-----------|------------
Statements | list of permissions

### Properties of a Statement

A policy statement must include one or more *actions*, which can be defined *inline* with the `Actions` property or *by reference* through a role definition, specified in the `Role` property.
Statements are typically composed using roles, but inline policy statements are useful for managing access to specific resources for special cases, or for when you don't want to create a named role for monolithic policies.

Property   | Description
-----------|-----------------------------
Actions    | list of operations, e.g., read IAM users, get compliance profiles, update node status, etc.
Role       | a named set of actions
Effect     | ALLOW or DENY
Projects   | list of project IDs to constrain the actions and/or role

### Properties of a Role

A v2.1 role consists of a list of **actions**.
Roles are discussed in detail in [Basic Role-Based Access Control]({{< relref "iam-v2-overview.md#basic-role-based-access-control" >}}).

Property   | Description
-----------|-----------------------------
Actions    | list of operations the role manages, e.g., read IAM users, get compliance profiles, update node status, etc.

## Members and Policies

A **member**  (previously called a *subject* in v1) may be a user, a team, or a token.
Users and teams may be *local*, meaning they are defined within Chef Automate, or managed by an external identity provider, specifically LDAP or SAML.
In this so far, v2.1 and v1 behave the same.

What is new in v2.1 is that policy *membership* is separate and distinct from policy *definition*.
(Notice that members were **not** included as part of the [New Policy Definition]({{< relref "iam-v2-overview.md#new-policy-definition" >}})).
Notably, with IAM v2.1, you can modify policy membership for any policy,
but you can only adjust policy definition for policies that you create yourself.
[Policy Types]({{< relref "iam-v2-overview.md#policy-types" >}}) discusses this point further.

## Policy Types

IAM v2.1 distinguishes two types of policies: *Chef-managed* and *custom*.
*Chef-managed* policies are provided by Chef and are integral to the operation of Chef Automate. The policy statements (comprising the policy definition) in Chef-managed policies are immutable.
*Custom* policies are those you create for your own needs. You can add, edit, and delete policy statements in your custom policies.

You can modify the membership of either type of policy.
It is, however, not possible to remove the local Administrator from the Administrator policy.

## Basic Role-Based Access Control

IAM v2.1 also introduces basic roles as our first step to delivering
Role-Based Access Control ([RBAC](https://en.wikipedia.org/wiki/Role-based_access_control)).
A role is a named set of actions.
This provides the benefits of *encapsulation* (only needing to know the name and not be encumbered by all the details after initial definition) and *reuse* (apply the role to any statement that needs it).
For example, one of the default roles provided is `Viewer`.
That role is comprised of a number of actions.
You could enumerate those actions in the `Actions` property of a given statement, or you can specify the single role name `Viewer` in the `Role` property of the statement; the result is identical either way.

Chef Automate ships with the default roles shown below.
To see the actions comprising the roles see [Chef-managed Roles]({{< relref "iam-v2-api-reference.md#roles" >}}) or inspect the roles in your browser.

Role          | Description
--------------|------------
Viewer        | can **view** everything in the system *except* IAM
Editor        | can **do** everything in the system *except* IAM
Owner         | can **do** everything in the system *including* IAM
Project Owner | Editor + can **view** and **assign** IAM resources for specific projects
Ingest        | can ingest data into the system and get resources needed to generate data e.g., profiles (mainly used for tokens)

Just as policies can be *Chef-managed* or *custom*, so too, can roles.
These default roles are Chef-managed, so they, like Chef-managed policies, are immutable.

You can use these default roles in your own policies, or create your own roles for more customized permissions.
Any roles you create can be modified later if you so choose.

Chef Automate also ships with several default policies that leverage these default roles.
The default policies are: Viewers, Editors, Administrator, and Ingest.

Just add members to any of these default policies, as described later, to quickly get your basic users up and running.

## Working with Projects

IAM projects are collections of resources created in Chef Automate or ingested from external data providers, including ChefInfra and ChefInspec.
Projects are used in a policy to reduce the scope of permissions of that policy to just the resources tagged with the given projects.
Without a policy statement to allow an operation tagged with a given project, a user cannot perform the actions of that policy.
Note that Chef Automate is currently limited to six projects while we continue to refine the user experience during this beta period.

There are four steps to setting up projects:

1. Define each project via the UI (or the command line if you prefer).
   Start on the projects list page shown previously to create your projects.
2. Create or modify IAM policy statements to restrict permissions to specific projects as needed.
   *Every* statement must either indicate specific projects or specify the wildcard denoting all projects (`*`).
   By default, any previously existing policies are automatically setup with that wildcard so they apply to all projects.
3. Tag resources with projects (see [Tagging Resources]({{< relref "iam-v2-overview.md#tagging-resources" >}})).
4. Select the projects to filter in the UI.
   Once you create some projects, use the **global project filter** in the penultimate position of the top navigation bar to select one or more projects for viewing.
   With no selection (the default), you are thereby saying you want *no* filtering, hence displaying resources for *all* projects for which you have permission.

Note that out-of-the-box we provide a *project-owner* role so the global admin may delegate much of these responsibilities for project management to others, to alleviate some of the burden for the global admin.

### Properties of a Project

A v2.1 project consists of a list of **rules**, where each rule describes a group of node characteristics.

Property   | Description
-----------|------------
Rules      | list of tagging rules

A rule consists of a list of **conditions**, where each condition describes a single characteristic.
It also contains a type where <<TODO describe what node and event mean>>

Property   | Description
-----------|------------
Type       | one of (node, event)
Conditions | list of node characteristics

A condition consists of these properties:

Property   | Description
-----------|------------
Attribute  | for **event** types: one of (CHEF_ORGANIZATION, CHEF_SERVER)
Attribute  | for **node** types: one of (CHEF_ORGANIZATION, CHEF_SERVER, ENVIRONMENT, CHEF_ROLE, CHEF_TAG, CHEF_POLICY_NAME, CHEF_POLICY_GROUP)
Operator   | one of (equals, member_of)
Values     | list of one or more values to match on the specified attribute

### Tagging Resources

There are two broad categories of resources that may be tagged: your set of nodes feeding data into Chef Automate and the set of IAM resources themselves.
Any of these that you do not explicitly tag (as described next) are considered *unassigned* with respect to projects.
We denote this group of unassigned projects as being in the `(unassigned)` project, including the parentheses as a way to connote that it is a special designator.
At the start, then, all of your resources are not tagged with any projects and thus can be viewed by selecting either `All Projects` or just the `(unassigned)` project in the global project filter.

Taggable IAM resources include teams, tokens, policies, and roles. Teams and tokens may be tagged with projects directly in the UI; policies and roles still can only be tagged on the command-line (since you cannot create or modify those at all in the UI yet).

Tagging your set of nodes is done via the rules you create for the project, mentioned just briefly above.
A rule specifies one or more conditions, and each condition specifies a set of attribute values that a node must satisfy to be tagged with the given project.
Once you define your set of projects with their contained rules and conditions, you then use the **Update Projects** button on the main projects list page to apply those definitions to all your nodes.

## Policies, Roles, and Projects in the UI

IAM v2.1 introduces partial policy management from within Chef Automate in your browser.
Chef Automate's **Settings** tab has a new *Access Management* heading on the left panel, with new pages for *Policies*, *Roles*, and *Projects*.

The main body on the policy list page displays all your policies along with their types (*Chef-managed* or *Custom*) and status (*In use* or *No members*).

![](/images/docs/admin-policies.png)

You can drill down into any resource listed. For example, here is the definition of the *Editors* policy after selecting it from the list.

![](/images/docs/admin-policies-editors-definition.png)

Notice also the **Members** tab: that allows you to manage the membership of the policy right from the browser.

![](/images/docs/admin-policies-editors-members.png)

Out of the box, the *Editors* policy includes the *editors* team as shown, but you are free to add other members with the **Add Members** button.
On that page you can add tokens as well as users and teams, be they local (defined within Chef Automate) or from an external identity provider (LDAP or SAML).
Because *local* users and teams are managed by Chef Automate directly, upon opening the *Add Members* page Automate lists all your local members but filters out those already attached to the policy, giving you a smaller list to peruse.
Observe here, for example, there is an *editor* user but there is no corresponding `editors` team in the list because, as shown above, that team is already a member.
By the way, that button at the bottom, **Add Member Expression**, is where you can add LDAP or SAML users and teams.

![](/images/docs/admin-policies-editors-add-members.png)

The role list page displays all your roles along with their types (*Chef-managed* or *custom*).
The inset image shows the resultant page that would display a role's details upon selecting it from the list.

![](/images/docs/admin-roles.png)

The project list page displays all your projects along with the status of associated rules (*No rules*, *Edits pending*, or *Applied*).
When you create or update rules, those changes are **not** directly applied to your production environment.
Rather, they are staged so that you may coordinate a set of changes to be applied together at a time of your choosing.
In fact, other administrative users may also stage changes and you would see each other's changes when you refresh your view.
All such changes will be applied together when you select the **Update Projects** button.

The inset image shows the resultant page that would display a project's details upon selecting it from the list of projects, and each project includes a list of rules.
You can drill down further into that list of rules to see the details of a rule, and an individual rule further consists of a list of one or more conditions.

![](/images/docs/admin-projects.png)
