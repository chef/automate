+++
title = "IAM v1 Overview"
aliases = [
    "/docs/authorization-overview/",
    "/docs/default-policies/",
    "/docs/iam-v1-policies/"
]
description = "IAM v1 Overview"
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authorization"
    weight = 40
+++

{{< warning >}}
IAM v1 is deprecated and we recommend updating to the latest version of Automate to make use of [IAM v2]({{< relref "iam-v2-overview" >}}).
{{< /warning >}}

This guide helps you understand and use Chef Automate's IAM v1 authorization system. 

## Overview

Chef Automate IAM v1 uses a policy-based authorization system.
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

## Structure

A policy is made up of three essential components: the **subjects**, which is
a list of users and teams, the **action** that those users/team members are allowed to perform,
and the **resource** on which they perform actions. Resources can be very granular,
but for most instances, restricting at the uppermost namespace will suffice.
A command to create a policy has this form:

```bash
curl -s -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"subjects":["subject1", "subject2"], "action":"whatever_action", "resource":"whatever_resource"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies?pretty
```

Here is a concrete example, granting `read` permission for the user `user@example.com`
and for the members of `team:ldap:ops` on resource `cfgmgmt:nodes:*`:

```bash
curl -s -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"subjects":["user:local:user@example.com", "team:ldap:ops"], "action":"read", "resource":"cfgmgmt:nodes:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies?pretty
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

A wildcard can replace any term of the subject; however, no terms can follow the wildcard.

Each user or team must specify its _provider_.

IAM v1 supports LDAP, SAML,
and local users or teams. 

### Resource

An `action` is performed on a `resource`.
Some types of resources are `nodes`, `compliance`, and `compliance:profiles`.

Resources are defined in hierarchical terms of increasing granularity and may have any number of terms.
With a wildcard (`*`), you can define policies with permission on any tier of the hierarchy.

### Wildcards

You can use a wildcard in two ways:

* as a standalone action, resource, or subject: `*`
* as the final term in an action, resource, or subject: `cfgmgmt:nodes:*`

A policy with a wildcard permits access to any action, resource, or subject in that space.

Authorization permits wildcards in policy definitions, so you can create a policy covering
a range of possible values.

## Default Authorization Settings

By default, Chef Automate IAM v1 is initialized with policies that allow the following:

* Admin users can perform any action on any Chef Automate IAM v1 resource.
* All non-Admin users can perform any action on Chef Automate IAM v1 resources,
  with the exception of actions on resources related to authorization (`auth`) or other system-level resources.
  Only Admins can modify teams, API tokens, other users, policies, notifications, and node lifecycle settings.

See the [IAM v1 Policies]({{< relref "#iam-v1-policies" >}}) documentation for more information
on how Chef Automate IAM v1's default policies map to different functions.

## Viewing Policies

### List All Existing Policies

Use the following `curl` command to list all existing policies:

```bash
curl -s -H "api-token: $TOKEN" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies?pretty
```

### List Specific Existing Policies

This example filters for the `compliance` resource:

```bash
curl -s -H "api-token: $TOKEN" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies | jq '.policies[] | select(.resource | startswith("compliance"))'
```

## Restrict Permissions on Resources

To remove a user's permission to act on a resource, find and delete the policy granting it permission, including any applicable top-level policies.
This may be a policy that gives permissions directly to a user, but more likely grants access indirectly through either user teams or wildcards. If the latter, be aware that your changes will affect other users as well.

Use the ID of the policy to delete it:

    ```bash
    curl -s -X DELETE -H "api-token: $TOKEN" -H "Content-Type: application/json" https://{{< example_fqdn "automate" >}}/api/v0/auth/policies/{id}?pretty
    ```

### Open Permissions on All Resources

Chef Automate IAM v1 by default allows all teams access to all parts of the
application except for administrative features.

If you would like to allow another team to access administrative features without
adding team members to the local `admins` team, you can create a policy allowing
access to all resources.

```bash
curl -s -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"subjects":["user:ldap:example_user@example.com"], "action":"*", "resource":"*"}' https:/{{< example_fqdn "automate" >}}/api/v0/auth/policies?pretty
```

### Permission for an API Client on Compliance Resources
To create a token that gives a client permission to read any Compliance resource.

1. Create an API token.

    ```bash
    curl -s -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"description":"My compliance token"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/tokens | jq .id

    95aef20b-0a4e-4698-bd69-ce2cf44c2e35
    ```

1. Copy the ID that is returned.

1. Create policies to permit that client to read `compliance:*`

    ```bash
    curl -s -H "api-token: $TOKEN" -H "Content-Type: application/json" -d '{"subjects":["token:95aef20b-0a4e-4698-bd69-ce2cf44c2e35"], "action":"read", "resource":"compliance:*"}' https://{{< example_fqdn "automate" >}}/api/v0/auth/policies?pretty
    ```

## IAM v1 Policies

### Event Feed

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
EventFeed | GetEventFeed | /eventfeed | GET | events | read
EventFeed | GetEventTypeCounts | /event_type_counts | GET | events:types | count
EventFeed | GetEventTaskCounts | /event_task_counts | GET | events:tasks | count
EventFeed | GetEventStringBuckets | /eventstrings | GET | events:strings | read
{{% /responsive-table %}}

### Applications

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Applications | GetServiceGroups | /applications/service-groups | GET | service_groups | list
Applications | GetServiceGroupsHealthCounts | /applications/service_groups_health_counts | GET | service_groups | list
Applications | GetServices | /applications/services | GET | service_groups| list
Applications | GetServicesBySG | /applications/service-groups/{service_group_id} | GET | service_groups | list
 |  |  |  |  |
JobScheduler | GetDisconnectedServicesConfig          | /retention/service_groups/disconnected_services/config        | GET  | service_groups:scheduler:disconnected_services        | read
JobScheduler | UpdateDisconnectedServicesConfig       | /retention/service_groups/disconnected_services/config        | POST | service_groups:scheduler:disconnected_services        | configure
JobScheduler | GetDeleteDisconnectedServicesConfig    | /retention/service_groups/delete_disconnected_services/config | GET  | service_groups:scheduler:delete_disconnected_services | read
JobScheduler | UpdateDeleteDisconnectedServicesConfig | /retention/service_groups/delete_disconnected_services/config | POST | service_groups:scheduler:delete_disconnected_services | configure
{{% /responsive-table %}}

### Client Runs

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ConfigMgmt | GetNodes | /cfgmgmt/nodes | GET | cfgmgmt:nodes | read
ConfigMgmt | GetRuns | /cfgmgmt/nodes/{node_id}/runs | GET | cfgmgmt:nodes:{node_id}:runs | read
ConfigMgmt | GetNodesCounts | /cfgmgmt/stats/node_counts | GET | cfgmgmt:stats:node_counts | read
ConfigMgmt | GetRunsCounts | /cfgmgmt/stats/run_counts | GET | cfgmgmt:stats:run_counts | read
ConfigMgmt | GetNodeRun | /cfgmgmt/nodes/{node_id}/runs/{run_id} | GET | cfgmgmt:nodes:{node_id}:runs:{run_id} | read
ConfigMgmt | GetSuggestions | /cfgmgmt/suggestions | GET | cfgmgmt:nodes | read
ConfigMgmt | GetOrganizations | /cfgmgmt/organizations | GET | cfgmgmt:nodes | read
ConfigMgmt | GetSourceFqdns | /cfgmgmt/source_fqdns | GET | cfgmgmt:nodes | read
ConfigMgmt | GetAttributes | /cfgmgmt/nodes/{node_id}/attribute | GET | cfgmgmt:nodes:{node_id}:attribute | read
ConfigMgmt | GetVersion | /cfgmgmt/version | GET | service_info:version | read
ConfigMgmt | GetPolicyCookbooks | /cfgmgmt/policy_revision/{revision_id} | GET | cfgmgmt:nodes:{revision_id} | read
 |  |  |  |  |
JobScheduler | GetStatusJobScheduler | /retention/nodes/status | GET | cfgmgmt:scheduler:job:status | read
JobScheduler | ConfigureNodesMissingScheduler | /retention/nodes/missing-nodes/config | POST | cfgmgmt:scheduler:missing-node | configure
JobScheduler | ConfigureDeleteNodesScheduler | /retention/nodes/delete-nodes/config | POST | cfgmgmt:scheduler:delete-node | configure
JobScheduler | ConfigureMissingNodesForDeletionScheduler | /retention/nodes/missing-nodes-deletion/config | POST | cfgmgmt:scheduler:missing-nodes-deletion | configure
{{% /responsive-table %}}

### Compliance

#### Compliance Reports

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ReportingService | ListReports | /compliance/reporting/reports | POST | compliance:reporting:reports | search
ReportingService | ReadReport | /compliance/reporting/reports/id/{id} | POST | compliance:reporting:reports:{id} | read
ReportingService | ListSuggestions | /compliance/reporting/suggestions | POST | compliance:reporting:suggestions | search
ReportingService | ListProfiles | /compliance/reporting/profiles | POST | compliance:reporting:profiles | search
ReportingService | ReadNode | /compliance/reporting/nodes/id/{id} | GET | compliance:reporting:nodes:{id} | read
ReportingService | ListNodes | /compliance/reporting/nodes/search | POST | compliance:reporting:nodes | search
ReportingService | GetVersion | /compliance/reporting/version | GET | compliance:reporting:version | read
{{% /responsive-table %}}

##### Compliance Reports Stats

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
StatsService | ReadSummary | /compliance/reporting/stats/summary | POST | compliance:reporting:stats:summary | read
StatsService | ReadTrend | /compliance/reporting/stats/trend | POST | compliance:reporting:stats:trend | read
StatsService | ReadProfiles | /compliance/reporting/stats/profiles | POST | compliance:reporting:stats:profiles | read
StatsService | ReadFailures | /compliance/reporting/stats/failures | POST | compliance:reporting:stats:failures | read
{{% /responsive-table %}}

##### Compliance Reports Nodes

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
NodesService | Create | /nodes | POST | nodes | create
NodesService | Read | /nodes/id/{id} | GET | nodes:{id} | read
NodesService | Update | /nodes/id/{id} | PUT | nodes:{id} | update
NodesService | Delete | /nodes/id/{id} | DELETE | nodes:{id} | delete
NodesService | List | /nodes/search | POST | nodes | list
NodesService | Rerun | /nodes/rerun/id/{id} | GET | nodes:{id} | rerun
{{% /responsive-table %}}

#### Compliance Scan Jobs

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
JobsService | Create | /compliance/scanner/jobs | POST | compliance:scanner:jobs | create
JobsService | Read | /compliance/scanner/jobs/id/{id} | GET | compliance:scanner:jobs:{id} | read
JobsService | Update | /compliance/scanner/jobs/id/{id} | PUT | compliance:scanner:jobs:{id} | update
JobsService | Delete | /compliance/scanner/jobs/id/{id} | DELETE | compliance:scanner:jobs:{id} | delete
JobsService | List | /compliance/scanner/jobs/search | POST | compliance:scanner:jobs | search
JobsService | Rerun | /compliance/scanner/jobs/rerun/id/{id} | GET | compliance:scanner:jobs:{id} | rerun
{{% /responsive-table %}}

#### Compliance Profiles

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ProfilesService | Read | /compliance/profiles/read/{owner}/{name}/version/{version} | GET | compliance:profiles:storage:{owner}:{name}:{version} | read
ProfilesService | ReadFromMarket | /compliance/market/read/{name}/version/{version} | GET | compliance:profiles:market:{name}:{version} | read
ProfilesService | Delete | /compliance/profiles/{owner}/{name}/version/{version} | DELETE | compliance:profiles:{owner}:{name}:{version} | delete
ProfilesService | List | /compliance/profiles/search | POST | compliance:profiles | search |
{{% /responsive-table %}}

### Secrets

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
SecretsService | Create | /secrets | POST | secrets | create
SecretsService | Read | /secrets/id/{id} | GET | secrets:{id} | read
SecretsService | Update | /secrets/id/{id} | PATCH | secrets:{id} | update
SecretsService | Delete | /secrets/id/{id} | DELETE | secrets:{id} | delete
SecretsService | List | /secrets/search | POST | secrets | search
{{% /responsive-table %}}

### Node Integrations

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
NodeManagerService | Create | /nodemanagers | POST | nodemanagers | create
NodeManagerService | Read | /nodemanagers/id/{id} | GET | nodemanagers:{id} | read
NodeManagerService | Update | /nodemanagers/id/{id} | PUT | nodemanagers:{id} | update
NodeManagerService | Delete | /nodemanagers/id/{id} | DELETE | nodemanagers:{id} | delete
NodeManagerService | List | /nodemanagers/search | POST | nodemanagers | list
NodeManagerService | SearchCloudProviderFields | /nodemanagers/id/{node_manager_id}/search-fields | POST | nodemanagers:{node_manager_id}:fields | search
NodeManagerService | SearchCloudProviderNodes | /nodemanagers/id/{node_manager_id}/search-nodes | POST | nodemanagers:{node_manager_id}:nodes | search
{{% /responsive-table %}}

### Ingest

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ChefIngester | ProcessChefRun | /ingest/events/chef/run | POST | ingest:nodes:{entity_uuid}:runs | create
ChefIngester | ProcessChefAction | /ingest/events/chef/action | POST | ingest:actions | create
ChefIngester | ProcessNodeDelete | /ingest/events/chef/nodedelete | POST | ingest:nodes | delete
ChefIngester | GetVersion | /ingest/version | GET | service_info:version | read
 |  |  |  |  |
LegacyDataCollector | ProcessLegacyEvent | /events/data-collector | POST | ingest:unified_events | create
{{% /responsive-table %}}

### Telemetry

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Telemetry | GetTelemetryConfiguration | /config | GET | telemetry:config | read
{{% /responsive-table %}}

### User Profile

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
UsersMgmt | GetUserByUsername | /auth/users/{username} | GET | auth:users:{username} | read
UsersMgmt | UpdateSelf | /users/{username} | PUT | users:{username} | update
{{% /responsive-table %}}

### Administrative

#### Notifications

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Notifications | AddRule | /notifications/rules | POST | notifications:rules | create
Notifications | DeleteRule | /notifications/rules/{id} | DELETE | notifications:rules:{id} | delete
Notifications | UpdateRule | /notifications/rules/{id} | PUT | notifications:rules:{id} | update
Notifications | GetRule | /notifications/rules/{id} | GET | notifications:rules:{id} | read
Notifications | ListRules | /notifications/rules | GET | notifications:rules | read
Notifications | ValidateWebhook | /notifications/webhook | POST | notifications:rules | validate
Notifications | Version | /notifications/version | GET | service_info:version | read
{{% /responsive-table %}}

#### Users

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
UsersMgmt | GetUsers | /auth/users | GET | auth:users | read
UsersMgmt | GetUserByUsername | /auth/users/{username} | GET | auth:users:{username} | read
UsersMgmt | CreateUser | /auth/users | POST | auth:users | create
UsersMgmt | DeleteUserByUsername | /auth/users/{username} | DELETE | auth:users:{username} | delete
UsersMgmt | UpdateUser | /auth/users/{username} | PUT | auth:users:{username} | update
{{% /responsive-table %}}

#### Teams
Admins that are member of the admins team can perform any action on any resource including Authorization and Notifications resources, which are inaccessible to non-admins.

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Teams | GetVersion | /auth/teams/version | GET | service_info:version | read
Teams | GetTeams | /auth/teams | GET | auth:teams | read
Teams | GetTeam | /auth/teams/{id} | GET | auth:teams:{id} | read
Teams | CreateTeam | /auth/teams | POST | auth:teams | create
Teams | UpdateTeam | /auth/teams/{id} | PUT | auth:teams:{id} | update
Teams | DeleteTeam | /auth/teams/{id} | DELETE | auth:teams:{id} | delete
Teams | GetUsers | /auth/teams/{id}/users | GET | auth:teams:{id}:users | read
Teams | AddUsers | /auth/teams/{id}/users | POST | auth:teams:{id} | create
Teams | RemoveUsers | /auth/teams/{id}/users | PUT | auth:teams:{id} | delete
Teams | GetTeamsForUser | /auth/users/{id}/teams | GET | auth:users:{id}:teams | read
{{% /responsive-table %}}

#### Tokens

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
TokensMgmt | GetTokens | /auth/tokens | GET | auth:api_tokens | read
TokensMgmt | CreateToken | /auth/tokens | POST | auth:api_tokens | create
TokensMgmt | UpdateToken | /auth/tokens/{id} | PUT | auth:api_tokens:{id} | update
TokensMgmt | GetToken | /auth/tokens/{id} | GET | auth:api_tokens:{id} | read
TokensMgmt | DeleteToken | /auth/tokens/{id} | DELETE | auth:api_tokens:{id} | delete
{{% /responsive-table %}}

#### Authorization

{{% responsive-table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Authorization | GetVersion | /auth/policies/version | GET | service_info:version | read
Authorization | CreatePolicy | /auth/policies | POST | auth:policies | create
Authorization | ListPolicies | /auth/policies | GET | auth:policies | read
Authorization | DeletePolicy | /auth/policies/{id} | DELETE | auth:policies:{id} | delete
Authorization | IntrospectAll | /auth/introspect | GET | auth_introspection:introspect_all | read
Authorization | IntrospectSome | /auth/introspect_some | POST | auth_introspection:introspect_some | read
Authorization | Introspect | /auth/introspect | POST | auth_introspection:introspect | read
{{% /responsive-table %}}

