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

{{< info >}}
To get the best possible IAM v2 experience, Chef Automate should be running the latest version before upgrading to IAM v2.
{{< /info >}}

Perform the upgrade to IAM v2 with `chef-automate iam upgrade-to-v2`; the response should look something like:

```terminal
Migrating v1 policies...

Updating deployment configuration

Applying deployment configuration
  Started automate-gateway

Creating default teams Editors and Viewers...

Success: Enabled IAM v2
```

Note: To upgrade without porting existing policies, use the `--skip-policy-migration` flag: `chef-automate iam upgrade-to-v2 --skip-policy-migration`.

## View Policies

After you've logged in to Chef Automate, select the **Settings** tab in the top navigation bar, then select and locate the `Policies` section on the left hand panel.

In this section, you can view all of your v2 policies. If you have upgraded without using the `--skip-policy-migration` flag, you will also see v1 policies.

This includes the following:

* New default (Chef-managed) policies: Administrator, Ingest, Editors, and Viewers.
* Imported v1 default policies--now called *legacy policies*--in the new v2 policy format and marked with the `[Legacy]` prefix.
* Imported v1 custom policies that you created; these are marked with the `[Legacy]` prefix and a `(custom)` suffix.

![](/images/docs/admin-policies-migrated.png)

## Policy Conversion

Now that you are up-and-running with v2 policies, as the first step we recommend that you reconstitute your v1 policies as v2 policies.
Once that is done, delete the old legacy v1 policies and you will have a clean, up-to-date system.

{{% info %}}
To delete a legacy policy, open the menu on any custom policy, located at the end of the policy row, and select **Delete Policy** from the menu.
(You will not have this option for Chef-managed policies.)

A warning appears if members are still attached to the policy because deleting that policy disrupts access for all of its members.
However, you can still delete the policy.
{{% /info %}}

Alternately, if you include `--skip-policy-migration` on upgrade, no existing v1 policies will be ported.
You will still need to create new v2 policies to preserve any IAM behavior from v1.

{{% warning %}}
Note that several legacy policies (Compliance Profile Access and Ingest Access) have API tokens that will stop working if not ported.
Alternately, you may choose to keep those policies intact if that's easier and you do not plan to alter those permissions.
{{% /warning %}}

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
In this case, you could create a policy called `Team Devops Managers`, which grants its members some--but not all--administrative privileges.
Create a JSON file in the editor of your choice.
Following JSON syntax, begin with the name property.
(For this example's complete JSON, see the end of this section.)

```json
  "name": "Team Devops Managers",
```

That name is for human consumption; when you want to refer to the policy in commands you will need to know its ID.
So let's give this policy the ID `team-managers-devops`.

```json
  "id": "team-managers-devops",
```

Additionally, we can permission actions on this policy just like any other IAM resource by assigning it to one or more projects.
If we leave the projects array empty, we indicate the policy is _unassigned_.
For example, anyone with permission to view _unassigned_ policies can view this policy.

```json
  "projects": [],
```

Let's further assume you want user `Bob` as well as anyone on team `gamma` to be authorized by this policy; this comprises the `members` array.

```json
  "members": [
    "user:local:bob",
    "team:local:gamma",
  ],
```

Next, you'll specify the permissions themselves--which in IAM v2 are the `statements`-- declared as an array.
The statement allows us to specify the `actions` a user is permitted to take on resources that have been assigned to a `project`.
The `projects` field on a statement is an array that may contain more than one existing project, a wildcard `*` to indicate permission to resources in _any project_, or the word "(unassigned)" to indicate permission to resources that have not been assigned to a project.
Note that the `projects` property in statements designates permission for the resources within the statement (here, that is `iam:users` and `iam:teams`), _not_ for the policy itself, and _cannot_ be left empty.
For more about projects, please see [Projects]({{< relref "iam-v2-guide.md#projects" >}}) or [Notes on the Projects Property](#notes-on-the-projects-property).

In this case, we only need a single statement providing access to the _get_, _list_, and _update_ actions for _users_ and _teams_ that have been assigned to the project `project-devops`.

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
      ],
      "projects": ["project-devops"],
    },
```

The complete policy should look like:

```json
{
  "name": "Team Devops Managers",
  "id": "team-managers-devops",
  "projects": [],
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
      ],
      "projects": ["project-devops"]
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

### Projects

Projects are used to group and permission Chef Automate resources as well as ingested data, specifically Compliance reports, Chef Server and Compliance events, and Infrastructure nodes.

Projects can be managed via the Projects list under the *Settings* tab and consist of an ID, a name, and a collection of rules. Project ingest rules are lists of conditions used only when
[assigning ingested resources to projects]({{< relref "iam-v2-guide#assigning-ingested-resources-to-projects" >}}),
so they are not relevant when assigning IAM resources such as teams or roles.

#### Creating a Project

To create a project, navigate to the Projects list under the **Settings** tab and select **Create Project**. You will need to provide a name and can optionally edit the ID. You must create a project before you can assign any resources to it.

#### Assigning Resources to Projects

You can use the browser to assign teams and tokens to projects. To assign a team to projects, select a team from the _Teams_ list, then select **Details**.
Likewise, to assign a token to projects, select a token from the API tokens list, then select **Details**.
In either case, you can select projects from the projects dropdown to assign.

You may also assign teams and tokens to projects on creation. In the creation modal, select any projects to which the new resource should belong.

Presently, policies and roles can only be assigned to projects using the CLI, not the browser. Users cannot be assigned to projects from the browser or CLI.

If you would like to delegate ownership of a project to another user so that they may assign resources, you will want to make that user a [Project Owner]({{< relref "iam-v2-guide.md#project-owners" >}}) of that project.

#### Assigning Ingested Resources to Projects

While Automate's local teams and tokens can be directly assigned to a project, ingested resources are created outside of Automate and therefore must be assigned to projects a different way.

Project rules are used to associate ingested resources with projects within Automate. A project rule contains conditions that determine if an ingested resource should be moved into the rule's project.
Each condition contains an attribute, operator, and value. See [Project Rules]({{< relref "iam-v2-api-reference.md#project-rules" >}}) for details on how to manage project rules.

In this example, after [creating a project]({{< relref "iam-v2-guide.md#creating-a-project" >}}) with the ID `project-devops`, you will add a project rule to this new project.
You will update projects to apply this new project rule, causing all matching ingested resources to be associated with `project-devops`.
You will then use the global project filter to filter your ingested data by `project-devops`.

First, determine which ingested resources should belong to the project. In this example, we want to add the following ingested resources to `project-devops`:

* Compliance reports with Chef Organization `devops`
* Infrastructure nodes with Environment `dev` and Chef Tag `devops-123`
* actions on Chef Servers `devops.pizza` or `devops.dog`

You may want to verify that those filters work as expected using the search filter bars on the Event Feed, Client Runs, and
Reports pages.

Navigate to the project details page of `project-devops`, either by heading directly to `https://{{< example_fqdn "automate" >}}/settings/projects/project-devops` or selecting the project name on the project list page.

Select the `Create Rule` button to create a new project rule. Choose resource type `Node` then fill in the first condition's fields.
Feel free to create fake ingest data that corresponds to the example json below, or come up with a condition that matches your existing data set.

{{% warning %}}
Compliance reports must be using **audit cookbook 7.5+** in order to make use of all of the available project rule attributes. Older reports will only have **Environment** and **Chef Role** available as attributes.
{{% /warning %}}

```json
{
  "id": "devops-rule",
  "name": "devops rule",
  "type": "NODE",
  "project_id": "project-devops",
  "conditions": [
    {
      "operator": "EQUALS",
      "attribute": "CHEF_ORGANIZATION",
      "values": [
        "devops"
      ]
    }
  ]
}
```

![](/images/docs/create-project-rule.png)

Save the rule. If you later need to change the name or the conditions, select the project rule name on the project details page.

You should see a message that says `Edits are pending: update projects to apply edits.` Select the `projects` link in the message to go back to the project list page.

On the project list page, the button `Update Projects` should be enabled since a new rule has been added to a project. You should also see `Edits pending` in the Ingest Rules column of `project-devops` in the projects table. Select `Update Projects` and confirm the action in the modal that pops up.

![](/images/docs/update-projects-button.png)

Updating a project begins an operation that applies all pending rule edits and then moves ingested resources into the correct projects according to those latest changes. An ingested resource is moved into a project if it matches at least one of the project's rules.
In this example, upon successful update, all ingested resources whose Chef Organization matches `devops` will be considered a part of the project `project-devops`.
Only these resources will appear in Automate's dashboards when the `project-devops` project has been selected in the global project filter.

As the operation takes place, you should see a percentage count up within the `Update Projects` button. You may cancel the update at any time by selecting
`Stop Updating Projects` and confirming the cancel in the modal that pops up.

{{% warning %}}
Avoid stopping an update unless absolutely necessary. It will leave your system in an unknown state where only some resources have been moved into their updated projects while others still remain in old projects. Only another successful update will restore the system to a happy state.
{{% /warning %}}

Once rules have been successfully applied, the update button will change to `Projects Up-to-Date` and be disabled until the next
time there are *pending edits* to any project.

To verify that the ingested resources have been moved into the correct projects, select `project-devops` in the global projects filter, which is on the top navbar. The data in Automate will now be filtered by the selected project.
In this example, the effect is revealed by navigating to the Compliance Reports' Nodes tab, which only features nodes that belong to the `devops` Chef Organization.

![](/images/docs/global-projects-filter.png)

Now that we have the first set of our ingested data associated with our new project, let's add another condition and a new rule to
add more data to `project-devops`.

{{% info %}}
Compliance and Infrastructure ingested resources are not the exact same nodes, so their properties may not be the same.
Separate conditions governing said resources *may* need to be used if their properties do not match exactly.

Similarly, ingested events require conditions of `Event` type to be associated with the correct project. A condition of type `Node` will not match an event, even if the condition's
operator, attribute, and value all match exactly (and vice versa with `Event` project rules and nodes).
{{% /info %}}

Return again to the project details page `https://{{< example_fqdn "automate" >}}/settings/projects/project-devops` and create two new rules by selecting `Create Rule`. Creating new rules will expand the data set under `project-devops`,
because an ingested resource need only match one rule to be placed in the project.

The first rule should contain two conditions.
Fill in the first condition with attribute `Environment`, operator `equals`, and value `dev`, or any value matching your data set.
Select `Add Condition` to add another condition with attribute `Chef Tag`, operator `equals`, and `devops-123`.
Save the rule.

{{% info %}}
Adding conditions further restricts the ingested data because every condition must be true for an ingested resource to be placed in the project.
{{% /info %}}

For the second rule, choose resource type `Event`. Fill in the first condition with attribute `Chef Server`, operator `member of`, and value `devops.pizza, devops.dog`, or any values matching your data set.

{{% info %}}
Note that setting the project rule `Resource Type` determines what condition attributes are available to select. `Event` rule conditions can only have the attributes `Chef Organization` or `Chef Server`.

Rules of type `Node` can have conditions with attributes `Chef Organization`, `Chef Server`, `Environment`, `Chef Role`, `Chef Tag`, `Chef Policy Name`, `Chef Policy Group`.
{{% /info %}}

Navigate to the project list page once more. Select `Update Projects`. Upon completion of the update, you should be able to
filter by `project-devops` across Automate's dashboards and see only the ingested data that you expect.

#### Project Owners

The role Project Owner is designed to allow admin users to delegate management of project membership to another user without granting that user access to all parts of Chef Automate. Project Owners have the following permissions on resources assigned to their project:

```text
"infra:*",
"compliance:*",
"system:*",
"event:*",
"ingest:*",
"secrets:*",
"telemetry:*",
"iam:projects:list",
"iam:projects:get",
"iam:projects:assign",
"iam:policies:list",
"iam:policies:get",
"iam:policyMembers:*",
"iam:teams:list",
"iam:teams:get",
"iam:teamUsers:*",
"iam:users:get",
"iam:users:list"
```

In order to create a Project Owner, you can use an existing user, or create a user with username `test_project_owner` by following [Creating Users]({{< relref "users.md#creating-users" >}}).

Next, you will create a policy that gives `test_project_owner` the role of `project-owner` for your new project. The policy will look like the JSON below:

```json
{
  "name": "Project Devops Policy",
  "id": "project-devops-policy",
  "projects": ["project-devops"],
  "members": [ "user:local:test_project_owner"],
  "statements": [
    {
      "effect": "ALLOW",
      "role": "project-owner",
      "projects": ["project-devops"]
    }
  ]
}
```

By adding `project-devops` to the policy's top-level `projects` field, we ensure that the Project Owner has access to this policy.

Save as a JSON file and follow the steps in [Creating a Policy]({{< relref "iam-v2-api-reference.md#creating-a-policy" >}}) to pass that data to Chef Automate.

#### Editor and Viewer Policies for Projects

You may additionally choose to create a `Project Devops Editors` policy and a `Project Devops Viewers`
policy. Once those are in place, you or your `project-devops` Owner can easily add members to each policy
to give them privileges to view or edit resources assigned to `Project Devops`. To create
those policies, use the JSON below:

Viewers Policy:

```json
{
  "name": "Project Devops Viewers",
  "id": "project-devops-viewer-policy",
  "projects": ["project-devops"],
  "members": [],
  "statements": [
    {
      "effect": "ALLOW",
      "role": "viewer",
      "projects": ["project-devops"]
    }
  ]
}
```

Editors Policy:

```json
{
  "name": "Project Devops Editors",
  "id": "project-devops-editor-policy",
  "projects": ["project-devops"],
  "members": [],
  "statements": [
    {
      "effect": "ALLOW",
      "role": "editor",
      "projects": ["project-devops"]
    }
  ]
}
```

Now, if you would like Terry to be able to view `Project Devops` and Kelly to be able to edit `Project Devops`, you'll add them
as members to the relevant policies. You can do that directly in the above JSON when creating the policy, or in the browser
by selecting **Settings**, then **Policies**, then `Project Devops Policy`. Under the **Details** tab, you will find the
option to select new policy members.

Assuming Terry is not a member of any other policy, once you make Terry a member of the `Project Devops Viewers` policy,
they will only be able to see resources assigned to `Project Devops`. They will not be able to update or delete them.
Kelly, however, will be able to do both.

See [Policy Membership]({{< relref "iam-v2-api-reference.md#policy-membership" >}}) for more information on policy membership.

## Reverting to IAM v1

Should you have a need to revert back to IAM v1, it is quick and easy to do:

```console
$ chef-automate iam reset-to-v1
```

Note that this discards your IAM v2 policies, roles, and projects, and re-configures Chef Automate to use your v1 policies again.
