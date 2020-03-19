+++
title = "IAM v2 API Reference"
description = "IAM v2 API Reference"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authorization"
    weight = 50
+++

This API reference details Chef Automate IAM v2 features from the command line.
If you are not already on IAM v2, [upgrade to IAM v2]({{< relref "iam-v2-guide.md#upgrade-to-iam-v2" >}}) first to ensure expected outcomes when using `curl` commands.

## Getting Started

Keep in mind the following context when exploring this API reference:

1. If you do not have one already, generate an admin token for IAM v2.
   To do so, run this command, filling in a token name of your choice:

   ```bash
   export TOKEN=`chef-automate iam token create <your-token-name-here> --admin`
   ```

2. URIs are relative to `https://<your-domain-here>/apis/iam/v2`, unless otherwise noted.

3. Attach the `?pretty` query string to an endpoint to get pretty-printed output.

Putting all of the above information together with the `/policies` endpoint as an example, this command fetches the list of policies with multi-line, formatted output:

```bash
curl -sH "api-token: $TOKEN" \
  https://{{< example_fqdn "automate" >}}/apis/iam/v2/policies?pretty
```

For those API methods that take JSON data, typically through the `create` and `update` methods,
you can provide that data to the REST endpoint in a variety of ways.
If the data is a relatively small size payload, you can include it in a `curl` command inline. For example:

```bash
curl -sH "api-token: $TOKEN" -d '<your JSON here>' ...
```

If the payload is larger, it is often convenient to store the data in a file, then pass that file. For example:

```bash
curl -sH "api-token: $TOKEN" -d @policy.json ...
```

## Projects Property

Teams, tokens, roles, and policies all contain a top-level `projects` property. You will see that in the example JSON for each resource in the following sections.
This `projects` property associates the particular IAM resource with one or more projects.
Users must have permissions for those projects to be able to view or modify those resources.
If the `projects` property is left empty, that resource is categorized as *unassigned*.

To create those permissions for users, there is a separate and distinct `projects` property on each policy statement.
This `projects` property may **not** be empty. You must either specify specific projects, or use a wildcard (`*`) to indicate all projects.

Type               | May be empty ?    | Allows "*" ?         | May specify (unassigned) ?
-------------------|-------------------|----------------------|---------------------------
top-level projects | empty allowed     | wildcard not allowed | (unassigned) not allowed
statement projects | empty not allowed | wildcard allowed     | (unassigned) allowed

## Policies and Roles

These endpoints are now documented in the [IAM Policies section](https://automate.chef.io/docs/api/#tag/Policies) and the [IAM Roles section](https://automate.chef.io/docs/api/#tag/Roles) of our API documentation.

### Default Chef-managed Roles

Name | ID| Actions
-----------------------|-----|--------
Owner              | owner         | \*
Viewer             | viewer        | secrets:\*:get, secrets:\*:list, infra:\*:get, infra:\*:list, compliance:\*:get, compliance:\*:list, system:\*:get, system:\*:list, event:\*:get, event:\*:list, ingest:\*:get, ingest:\*:list, iam:projects:list, iam:projects:get, applications:\*:list, applications:\*:get
Editor             | editor        | infra:\*, compliance:\*, system:\*, event:\*, ingest:\*, secrets:\*, telemetry:\*, iam:projects:list, iam:projects:get, iam:projects:assign, applications:\*
Project Owner      | project-owner | infra:\*, compliance:\*, system:\*, event:\*, ingest:\*, secrets:\*, telemetry:\*, iam:projects:list, iam:projects:get, iam:projects:assign, iam:policies:list, iam:policies:get, iam:policyMembers:\*, iam:teams:list, iam:teams:get, iam:teamUsers:\*, iam:users:get, iam:users:list
Ingest             | ingest        | infra:ingest:\*, compliance:profiles:get, compliance:profiles:list

## Projects and Project Rules

These endpoints are now documented in the [IAM Projects section](https://automate.chef.io/docs/api/#tag/Projects) of our API documentation.

## Teams

HTTP request              | Description
--------------------------|------------
GET /teams               | list all teams
GET /teams/{**id**}      | get the specified team
POST /teams              | create a team
PUT /teams/{**id**}      | update a team
DELETE /teams/{**id**}   | delete a team

### Example Team

```json
{
  "name": "team 1",
  "id": "team-1",
  "projects": [
    "east-region",
    "west-region"
  ]
}
```

### Listing Teams

```bash
curl -sSH "api-token: $TOKEN" -X GET \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams?pretty
```

### Getting a Team

```bash
curl -sSH "api-token: $TOKEN" -X GET \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}?pretty
```

### Creating a Team

Create a team by composing JSON with all the necessary team properties (see [Example Team]({{< relref "iam-v2-api-reference.md#example-team" >}})).

Assuming you store the JSON content in the file "team.json", pass the file to `curl` to create the team:

```bash
curl -sSH "api-token: $TOKEN" -d @team.json -X POST \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams?pretty
```

### Updating a Team

When updating a team, supply all of a team's properties, not just the ones you wish to update.
Properties that you do not include are reset to empty values.
The team ID is immutable and it can only be set at creation time.

```bash
curl -sSH "api-token: $TOKEN" -d @team.json -X PUT \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}?pretty
```

### Deleting a Team

Deleting a team is permanent and cannot be undone.

```bash
curl -sSH "api-token: $TOKEN" -X DELETE \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}?pretty
```

## Team Membership

A local team consists exclusively of local users.
Note that the `user_ids` of the local users used here are GUIDs, generated when you create users.
A user's GUID is reported in the `membership_id` property when you fetch a user.

HTTP request                           | Description
---------------------------------------|------------
GET /teams/{**id**}/users              | list the membership
POST /teams/{**id**}/users:add         | add to the membership
POST /teams/{**id**}/users:remove      | remove from the membership

### Example Team Membership Payload

```json
{
  "id": "team-1",
  "user_ids": [
    "1a5a63d7-0b14-465a-aec1-5eea08a72343",
    "6c4e3e8a-fade-4a9c-ac9f-99b86677530b"
  ]
}
```

### Listing Team Users

The output lists all local users on the specified team.

```bash
curl -sSH "api-token: $TOKEN"  -X GET \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}/users?pretty
```

### Adding Users to a Team

```bash
curl -sSH "api-token: $TOKEN" -X POST \
-d '{"user_ids":["{membership_id}","{membership_id}"]}' \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}/users:add?pretty
```

### Removing Users from a Team

The removed users still exists within Chef Automate, but are no longer associated with this team.

```bash
curl -sSH "api-token: $TOKEN" -X POST \
-d '{"user_ids":["{membership_id}","{membership_id}"]}' \
https://{{< example_fqdn "automate" >}}/apis/iam/v2/teams/{team-id}/users:remove?pretty
```

## Tokens

Token endpoints are now documented at [Identity and Access Management Tokens](https://automate.chef.io/docs/api/#tag/Tokens).
