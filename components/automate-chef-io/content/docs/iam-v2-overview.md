+++
title = "IAM v2 Overview"
description = "IAM v2 Overview"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authorization"
    weight = 30
+++
<!-- Goal
Show the simplest possible experience that most users will experience. We are assuming that experience will be UI only, so present the UI information first followed by CLI information presented in reference format.
-->

Chef Automate's Identity and Access Management (IAM v2) system is opt-in only, and Chef Automate users will not be automatically upgraded to IAM v2 from IAM v1.

We designed IAM v2 to leave your v1 policy data untouched during your upgrade to v2; however, you can choose to not migrate v1 policies by using the provided `--skip-policy-migration` flag with the upgrade command.

Whether or not you migrate your v1 policies, if at any time you decide to revert to v1, your original v1 policies will still be intact.
Reverting to v1 will remove any new v2 policies, roles, and projects created while using IAM v2.
Users, teams, and API tokens are shared between v1 and v2, so changes to them will persist.

## New Features in IAM v2

IAM v2 policies allow multiple permissions, separating out policy membership from policy definition for fine-grained control, and includes roles for role-based access control.
Additionally, IAM v2 allows policy members to be managed directly from the Automate UI.
Perhaps most significantly, IAM v2 supports projects, which allow for filtering and segregation of your data amongst your user base.

At the heart of Chef Automate's IAM system is the *policy*.
A policy defines permissions for who may perform what action on which resource scoped by project.
The "who" may be a user, a team, or a system.
Users and teams are designated by name while systems use pre-authorized API tokens to communicate with Automate.

The following diagram shows the new policy structure. We will detail the specifics in the next few sections.

![](/images/docs/iam-v2-diagram.png)

## New Policy Definition

IAM v2 uses multi-statement policies, which support complex permissions in a single policy, and results in fewer policies required to secure your system.
Each statement specifies a single permission.
The net effect (ALLOW or DENY) of a policy is determined by evaluating the effect of each statement and combining them. If there is at least one statement allowing access, and none denying it, then the policy allows access. Otherwise, access is denied.

Access is granted only if explicitly allowed in some policy and not explicitly denied in any policy.
IAM policy evaluation order is:

1. By default, all requests are denied.
2. Any explicit ALLOW overrides the default DENY.
3. Any explicit DENY in any policy overrides any ALLOWs in any policy.

### Net Effect Examples

The following examples illustrate the net effect of a policy and if access is granted to a user:

- Neither user1, nor any team that user1 belongs to, are included in any policy with the action `iam:users:list`.
  _Net effect_: user1 is denied access to view the list of users.
  This example illustrates the global default DENY condition.
- Bob is a member of team alpha and team omega.
  Team alpha has a broad range of permissions while team omega is very restricted in what its members may access.
  The "major teams" policy has a statement that allows team alpha to access the resource `compliance:reporting:nodes`, while it has another statement that denies Team Omega access to that very same resource.
  _Net effect_: Bob is denied access because there is at least one statement denying access.
- Mary is a member of the default viewers team, which allows read access to everything because of the default Viewers policy.
  Mary is also a member of the deployment team, which has permission to upload and download profiles.
  _Net effect_: Mary is granted both those permissions, which is effectively the union of relevant policies.

### Properties of a Policy

An IAM v2 policy consists of a list of **statements**, where each statement describes a permission.

### Properties of a Statement

A policy statement must include a `Role`, or a list of inline `Actions`.

Property   | Description
-----------|-----------------------------
Role       | a named list of actions
Effect     | ALLOW or DENY
Projects   | list of project IDs to constrain the role

### Properties of a Role

An IAM v2 role consists of a list of **actions**. Some examples of those actions include listing IAM users, getting compliance profiles, or updating node status.
Roles are discussed in detail in [Role-Based Access Control]({{< relref "iam-v2-overview.md#role-based-access-control" >}}).

## Members and Policies

A **member**, which was called a *subject* in v1, may be a user, a team, or an API token.
Users and teams may be *local*, meaning they are defined within Chef Automate, or managed by an external identity provider, specifically LDAP or SAML.

IAM v2 policy *membership* is separate and distinct from policy *definition*.
(Notice that members were **not** included as part of the [New Policy Definition]({{< relref "iam-v2-overview.md#new-policy-definition" >}}).)
Notably, with IAM v2, you can modify policy membership for any policy, but you can only adjust policy definition for *Custom* policies.

## Policy Types

IAM v2 distinguishes two types of policies: *Chef-managed* and *Custom*.
*Chef-managed* policies are provided by Chef and are integral to the operation of Chef Automate. The policy statements in Chef-managed policies cannot be changed.
*Custom* policies are policies that you create for your own needs. You can add, edit, and delete policy statements in your custom policies.

The membership can be modified in both types of policies.

## Role-Based Access Control

A role is a named list of actions.
This listing provides the benefits of *encapsulation*, where only the name is needed and is not encumbered by all the details after initial definition, and *reusability*, in applying the role to any statement that needs it.

Chef Automate has 5 default roles.

Role          | Description
--------------|------------
Viewer        | **View** everything in the system *except* IAM
Editor        | **Do** everything in the system *except* IAM
Owner         | **Do** everything in the system *including* IAM
Project Owner | Editor + **view** and **assign** projects
Ingest        | Ingest data into the system

Just like policies, roles are considered either *Chef-managed* or *Custom*.
These default roles are Chef-managed, so they cannot be changed, like Chef-managed policies.

You can use these default roles in your own policies, or create your own custom roles for more customized permissions.
Any roles you create can be modified later.

Chef Automate also ships with several default policies that leverage these default roles.
The default policies are: Viewers, Editors, Administrator, and Ingest.

## Working with Projects

IAM v2 projects are collections of resources either created in Chef Automate, or ingested from external data providers, including Chef Infra and Chef InSpec.
Projects are used in a policy to reduce the scope of that policy's permissions to only the resources assigned to the given projects.

{{< info >}}
Chef Automate limits you to 300 projects while we continue to refine the user experience.
See [Configuring Project Limit]({{< relref "iam-v2-guide.md#configuring-project-limit" >}}) for configuration instructions.
{{< /info >}}

### Setting Up Projects

IAM v2 projects can be setup using the following steps:

1. Define each project via the UI.
   Start on the _Projects List_ page to create your projects.
2. Create or modify IAM policy statements to restrict permissions to specific projects as needed.
   *Every* statement must either indicate specific projects, or specify the wildcard (`*`), which denotes all projects.
   By default, any previously existing policies are automatically setup with that wildcard so they apply to all projects.
3. [Assign resources to projects]({{< relref "iam-v2-overview.md#assigning-resources-to-projects" >}}).
4. Select the projects to filter in the UI.
   After creating projects, use the **global project filter** in the top navigation to select one or more projects for viewing.
   No selection means all resources for which you have permission will be displayed.

By default, Automate includes a *project-owner* role, so the global admin may delegate much of these responsibilities for project management to others and alleviate some of the burden for the global admin.

### Assigning Resources to Projects

There are two categories of resources that may be assigned to projects:

- Ingested client run and compliance nodes
- Teams, API Tokens, Policies, and Roles created in Automate

Any of these resources that you do not explicitly assign to a project are considered *unassigned* with respect to projects.
After upgrading and before creating projects, all of your resources are considered *unassigned*.

Assigning your set of ingested Client Run and Compliance Nodes to projects is done via the project ingest rules that you create for the project.
An ingest rule specifies one or more conditions, and each condition specifies a set of attribute values that a node must satisfy to be assigned to the given project.
Once you define your set of projects with their contained ingest rules and conditions, use the **Update Projects** button on the bottom of any page to apply those definitions.

Teams and API tokens may be assigned to projects directly in the UI. Policies and roles can only be assigned through the command line.
These resources created within Automate do not make use of, nor do they require, any project ingest rules.

Only some resources in Chef Automate respect projects. Only resources that respect projects will be filtered when using the project filter. Resources that do not respect projects will always be displayed and will ignore any applied project filters. After IAM v2 becomes generally available, we will continue the work to make more resources respect projects.

#### Resources that respect projects

- API Tokens
- Ingested Client Run nodes
- Ingested Compliance nodes
- Policies
- Teams
- Roles

#### Resources that do not yet respect projects

- Compliance Scan Jobs
- Compliance Profiles
- Data Feeds
- Habitat Services
- Manually added nodes
- Node Credentials
- Node Managers (also called Node Integrations)
- Notifications
- Users

### Properties of a Project

To assign a project to a set of ingested client run and compliance nodes, the project requires a list of **ingest rules**, where each rule describes a group of node characteristics.

#### Project Ingest Rules

A rule consists of a list of **conditions**, where each condition describes a single characteristic.
It also contains a type: *node* or *event*.
*node* corresponds to ingested client run and compliance nodes, and *event* corresponds to ingested events on the _Event Feed_ page.

Property   | Description
-----------|------------
Type       | node or event
Conditions | list of node characteristics

#### Project Ingest Rule Conditions

A condition consists of these properties:

Property               | Description
-----------------------|------------
Event Attribute        | Chef Organization or Chef Server
Node Attribute         | Chef Organization, Chef Server, Environment, Chef Role, Chef Tag, Chef Policy Name, or Chef Policy Group
Operator               | equals of member of
Values                 | list of one or more values to match on the specified attribute

## Policies, Roles, and Projects in the UI

Chef Automate's **Settings** tab has an *Access Management* heading in the left panel, with pages for *Policies*, *Roles*, and *Projects*.

The _Policy List_ page displays all your policies along with their types (*Chef-managed* or *Custom*) and status (*In use* or *No members*).

![](/images/docs/admin-policies.png)

Select any listed resource to view its details.
For example, here is the definition of the *Editors* policy after selecting it from the list.

![](/images/docs/admin-policies-editors-definition.png)

Notice also the **Members** tab, which allows you to manage the membership of the policy.

![](/images/docs/admin-policies-editors-members.png)

The *Editors* policy includes, by default, the *editors* team as shown, but you are free to add other members with the **Add Members** button.
On that page, you can add tokens as well as users and teams.
Users and teams may be local, LDAP or SAML.
Upon opening the _Add Members_ page, Automate lists all your local members but filters out those already attached to the policy.
For example, there is an *editor* user but no `editors` team in the member list because, that team is already a member.
The **Add Member Expression** button at the bottom, is where you can add LDAP or SAML users and teams.

![](/images/docs/admin-policies-editors-add-members.png)

The _Role List_ page displays all your roles along with their types (*Chef-managed* or *Custom*).
Selecting a role from the list opens the role's detail page, displaying the definition of the role.

![](/images/docs/admin-roles.png)

The _Project List_ page displays all your projects along with the status of associated project ingest rules (*No rules*, *Edits pending*, or *Applied*).
When you create or update ingest rules, those changes are staged and **not** directly applied.
Other users may also stage changes.
All changes will be applied together when you select the **Update Projects** button.

Selecting a project from the list opens the projects's detail page and displays the list of ingest rules comprising that project.
From there, you can select any individual rule to view its list of conditions, and then select a condition to view or update its details.

![](/images/docs/admin-projects.png)

## Projects in the API

It is also possible to filter APIs by project from the Chef Automate CLI. The following API requests are examples of fetching data using project headers as filters.

To use these API requests, first:

- [Create an admin token](https://automate.chef.io/docs/cli-chef-automate/#chef-automate-iam-token-create) and set the admin token to the environment variable `$TOKEN`
- [Create two projects](https://automate.chef.io/docs/api/#operation/CreateProject). In our examples below, we use two projects named `test-project-1` and `test-project-2`, respectively.
- [Assign IAM resources]({{< relref "iam-v2-guide.md#assigning-resources-to-projects" >}}) to both projects
- [Assign ingested resources]({{< relref "iam-v2-guide.md#assigning-ingested-resources-to-projects" >}}) to both projects

This API request returns a list of teams that belong to `test-project-1`.

```bash
curl -sH "api-token: $TOKEN" -H "projects: test-project-1" \
  https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams?pretty
```

This API request returns a list of tokens that belong to `test-project-2`.

```bash
curl -sH "api-token: $TOKEN" -H "projects: test-project-2" \
  https://{{< example_fqdn "automate" >}}/apis/iam/v2/tokens?pretty
```

This API request returns a list of Infrastructure nodes that do not belong to any project.
Note that the `(unassigned)` project does not need to be created.

```bash
curl -kH "api-token: $TOKEN" -H "projects: (unassigned)" \
  https://a2-dev.test/api/v0/cfgmgmt/nodes?pagination.page=1&pagination.size=100&sorting.field=name&sorting.order=ASC
```

This API request returns a list of Compliance nodes that belong to `test-project-1` or `test-project-2`.

```bash
curl  -kH "api-token: $TOKEN" -H "projects: test-project-1, test-project-2"  -X POST \
  https://a2-dev.test/api/v0/compliance/reporting/nodes/search?pretty
```
