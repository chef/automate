+++
title = "Default Policies"
description = "Chef Automate's default policies and the specific endpoints they protect"
weight = 23
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "authorization"
    weight = 20
+++

# Default Policies in Chef Automate

This page provides an in-depth look at each of Chef Automate's default policies and
the specific endpoints they protect.

## Configuration Management

### Configuration Management Policies

> These default policies allow all users to perform any action on Configuration Management resources

```bash
  {
      "action": "*",
      "resource": "cfgmgmt",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "cfgmgmt:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Configuration Management (Client Runs) Pages

> Corresponds to "Client Runs" tab or `/nodes`

{{% table %}}
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
{{% /table %}}

## Compliance

### Compliance Policies

> These default policies allow all users to perform any action on Compliance resources

```bash
  {
      "action": "*",
      "resource": "compliance:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Compliance Pages

> Corresponds to "Compliance" tab (`/compliance/reporting/overview`), "Scan Jobs" tab
(`/compliance/scanner/jobs`) & "Profiles" tab (`/profiles`)

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ProfilesService | Read | /compliance/profiles/read/{owner}/{name}/version/{version} | GET | compliance:profiles:storage:{owner}:{name}:{version} | read
ProfilesService | ReadFromMarket | /compliance/market/read/{name}/version/{version} | GET | compliance:profiles:market:{name}:{version} | read
ProfilesService | Delete | /compliance/profiles/{owner}/{name}/version/{version} | DELETE | compliance:profiles:{owner}:{name}:{version} | delete
ProfilesService | List | /compliance/profiles/search | POST | compliance:profiles | search
 |  |  |  |  |
ReportingService | ListReports | /compliance/reporting/reports | POST | compliance:reporting:reports | search
ReportingService | ReadReport | /compliance/reporting/reports/id/{id} | POST | compliance:reporting:reports:{id} | read
ReportingService | ListSuggestions | /compliance/reporting/suggestions | POST | compliance:reporting:suggestions | search
ReportingService | ListProfiles | /compliance/reporting/profiles | POST | compliance:reporting:profiles | search
ReportingService | ReadNode | /compliance/reporting/nodes/id/{id} | GET | compliance:reporting:nodes:{id} | read
ReportingService | ListNodes | /compliance/reporting/nodes/search | POST | compliance:reporting:nodes | search
ReportingService | GetVersion | /compliance/reporting/version | GET | compliance:reporting:version | read
 |  |  |  |  |
StatsService | ReadSummary | /compliance/reporting/stats/summary | POST | compliance:reporting:stats:summary | read
StatsService | ReadTrend | /compliance/reporting/stats/trend | POST | compliance:reporting:stats:trend | read
StatsService | ReadProfiles | /compliance/reporting/stats/profiles | POST | compliance:reporting:stats:profiles | read
StatsService | ReadFailures | /compliance/reporting/stats/failures | POST | compliance:reporting:stats:failures | read
 |  |  |  |  |
JobsService | Create | /compliance/scanner/jobs | POST | compliance:scanner:jobs | create
JobsService | Read | /compliance/scanner/jobs/id/{id} | GET | compliance:scanner:jobs:{id} | read
JobsService | Update | /compliance/scanner/jobs/id/{id} | PUT | compliance:scanner:jobs:{id} | update
JobsService | Delete | /compliance/scanner/jobs/id/{id} | DELETE | compliance:scanner:jobs:{id} | delete
JobsService | List | /compliance/scanner/jobs/search | POST | compliance:scanner:jobs | search
JobsService | Rerun | /compliance/scanner/jobs/rerun/id/{id} | GET | compliance:scanner:jobs:{id} | rerun
{{% /table %}}

## Event Feed

### Event Feed Policies

> These default policies allow all users to perform any action on Event Feed resources

```bash
  {
      "action": "*",
      "resource": "events",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "events:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Event Feed Page

> Corresponds to "Event Feed" tab (`/event-feed`)

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
EventFeed | GetEventFeed | /eventfeed | GET | events | read
EventFeed | GetEventTypeCounts | /event_type_counts | GET | events:types | count
EventFeed | GetEventTaskCounts | /event_task_counts | GET | events:tasks | count
EventFeed | GetEventStringBuckets | /eventstrings | GET | events:strings | read
{{% /table %}}

## Applications (BETA)

### Applications page

> These default policies allow all users to perform any action on application page resources

```bash
  {
      "action": "*",
      "resource": "service_groups",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "service_groups:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Applications Page

> Corresponds to "Application tab (`/applications`)

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Applications | GetServiceGroups | /beta/applications/service-groups | GET | service_groups | list
Applications | GetServiceGroupsHealthCounts | /beta/applications/service_groups_health_counts | GET | service_groups | list
Applications | GetServices | /beta/applications/services | GET | service_groups| list
Applications | GetServicesBySG | /beta/applications/service-groups/{service_group_id} | GET | service_groups | list
 |  |  |  |  |
JobScheduler | GetDisconnectedServicesConfig          | /beta/retention/service_groups/disconnected_services/config        | GET  | service_groups:scheduler:disconnected_services        | read
JobScheduler | UpdateDisconnectedServicesConfig       | /beta/retention/service_groups/disconnected_services/config        | POST | service_groups:scheduler:disconnected_services        | configure
JobScheduler | GetDeleteDisconnectedServicesConfig    | /beta/retention/service_groups/delete_disconnected_services/config | GET  | service_groups:scheduler:delete_disconnected_services | read
JobScheduler | UpdateDeleteDisconnectedServicesConfig | /beta/retention/service_groups/delete_disconnected_services/config | POST | service_groups:scheduler:delete_disconnected_services | configure
{{% /table %}}

## Telemetry

### TelemetryPolicies

> This default policy allow all users to perform any action on Telemetry resources

```bash
  {
      "action": "*",
      "resource": "telemetry:config",
      "subjects": [
          "user:*"
      ]
  }
```

### Telemetry access

> Corresponds to Telemetry (`/telemetry`)

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
Telemetry | GetTelemetryConfiguration | /config | GET | telemetry:config | read
{{% /table %}}

## Secrets

### Secrets Policies

> These default policies allow all users to perform any action on Secrets resources

```bash
  {
      "action": "*",
      "resource": "secrets",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "secrets:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Secrets (Credentials) Page

> Corresponds to the "Credentials" tab or `/compliance/credentials`

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
SecretsService | Create | /secrets | POST | secrets | create
SecretsService | Read | /secrets/id/{id} | GET | secrets:{id} | read
SecretsService | Update | /secrets/id/{id} | PATCH | secrets:{id} | update
SecretsService | Delete | /secrets/id/{id} | DELETE | secrets:{id} | delete
SecretsService | List | /secrets/search | POST | secrets | search
{{% /table %}}

## Nodes

### Nodes Policies

> These default policies allow all users to perform any action on Nodes resources

```bash
  {
      "action": "*",
      "resource": "nodes",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "nodes:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Nodes (Scanner) Page

> Corresponds to `/compliance/scanner/nodes`

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
NodesService | Create | /nodes | POST | nodes | create
NodesService | Read | /nodes/id/{id} | GET | nodes:{id} | read
NodesService | Update | /nodes/id/{id} | PUT | nodes:{id} | update
NodesService | Delete | /nodes/id/{id} | DELETE | nodes:{id} | delete
NodesService | List | /nodes/search | POST | nodes | list
NodesService | Rerun | /nodes/rerun/id/{id} | GET | nodes:{id} | rerun
{{% /table %}}

## Node Manager

### Node Manager Policies

> These default policies allow all users to perform any action on Node Manager resources

```bash
  {
      "action": "*",
      "resource": "nodemanagers",
      "subjects": [
          "user:*"
      ]
  },
  {
      "action": "*",
      "resource": "nodemanagers:*",
      "subjects": [
          "user:*"
      ]
  }
```

### Node Manager Page

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
NodeManagerService | Create | /nodemanagers | POST | nodemanagers | create
NodeManagerService | Read | /nodemanagers/id/{id} | GET | nodemanagers:{id} | read
NodeManagerService | Update | /nodemanagers/id/{id} | PUT | nodemanagers:{id} | update
NodeManagerService | Delete | /nodemanagers/id/{id} | DELETE | nodemanagers:{id} | delete
NodeManagerService | List | /nodemanagers/search | POST | nodemanagers | list
NodeManagerService | SearchCloudProviderFields | /nodemanagers/id/{node_manager_id}/search-fields | POST | nodemanagers:{node_manager_id}:fields | search
NodeManagerService | SearchCloudProviderNodes | /nodemanagers/id/{node_manager_id}/search-nodes | POST | nodemanagers:{node_manager_id}:nodes | search
{{% /table %}}

## Ingest

### Ingest Policy

> This default policy allows only clients, such as Chef Client, to perform any action on Ingest resources

*Notes:*

- No users may post data to these endpoints.

```bash
  {
      "action": "*",
      "resource": "ingest:*",
      "subjects": [
          "token:*"
      ]
  }
```

### Ingest API Endpoints (Internal)

> Corresponds to `/ingest/events`

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
ChefIngester | ProcessChefRun | /ingest/events/chef/run | POST | ingest:nodes:{entity_uuid}:runs | create
ChefIngester | ProcessChefAction | /ingest/events/chef/action | POST | ingest:actions | create
ChefIngester | ProcessNodeDelete | /ingest/events/chef/nodedelete | POST | ingest:nodes | delete
ChefIngester | GetVersion | /ingest/version | GET | service_info:version | read
 |  |  |  |  |
LegacyDataCollector | ProcessLegacyEvent | /events/data-collector | POST | ingest:unified_events | create
{{% /table %}}

## Profile

### Profile Policy

> These default policies allow all users to access their own profile and update it

*Notes:*

- `${a2:username}` denotes a policy variable
  that is filled in with the actual user name upon evaluation.

```bash
  {
      "action": "*",
      "resource": "users:${a2:username}",
      "subjects": [
          "user:local:*"
      ]
  },
  {
      "action": "read",
      "resource": "auth:users:${a2:username}",
      "subjects": [
          "user:local:*"
      ]
  }
```

### Profile Pages

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
UsersMgmt | GetUserByUsername | /auth/users/{username} | GET | auth:users:{username} | read
UsersMgmt | UpdateSelf | /users/{username} | PUT | users:{username} | update
{{% /table %}}

## Administrative

### Administrative Policy

> By default, only members of the admin team can perform actions on these resources.

*Notes:*

- Admins can perform any action on any resource, _including_ Authorization and
  Notifications resources, which are inaccessible to non-admins.

- This policy cannot be deleted

```bash
  {
      "action": "*",
      "resource": "*",
      "subjects": [
          "team:local:admins"
      ]
  }
```

### Admin & Notifications (Administrative) Pages

> Corresponds to the "Admin" (`/auth`) and "Notifications" tab (`/notifications`)

{{% table %}}
Service | Method | HTTP Endpoint | HTTP Method | Resource | Action
---|---|---|---|---|---
TokensMgmt | GetTokens | /auth/tokens | GET | auth:api_tokens | read
TokensMgmt | CreateToken | /auth/tokens | POST | auth:api_tokens | create
TokensMgmt | UpdateToken | /auth/tokens/{id} | PUT | auth:api_tokens:{id} | update
TokensMgmt | GetToken | /auth/tokens/{id} | GET | auth:api_tokens:{id} | read
TokensMgmt | DeleteToken | /auth/tokens/{id} | DELETE | auth:api_tokens:{id} | delete
 |  |  |  |  |
Notifications | AddRule | /notifications/rules | POST | notifications:rules | create
Notifications | DeleteRule | /notifications/rules/{id} | DELETE | notifications:rules:{id} | delete
Notifications | UpdateRule | /notifications/rules/{id} | PUT | notifications:rules:{id} | update
Notifications | GetRule | /notifications/rules/{id} | GET | notifications:rules:{id} | read
Notifications | ListRules | /notifications/rules | GET | notifications:rules | read
Notifications | ValidateWebhook | /notifications/webhook | POST | notifications:rules | validate
Notifications | Version | /notifications/version | GET | service_info:version | read
 |  |  |  |  |
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
 |  |  |  |  |
UsersMgmt | GetUsers | /auth/users | GET | auth:users | read
UsersMgmt | GetUserByUsername | /auth/users/{username} | GET | auth:users:{username} | read
UsersMgmt | CreateUser | /auth/users | POST | auth:users | create
UsersMgmt | DeleteUserByUsername | /auth/users/{username} | DELETE | auth:users:{username} | delete
UsersMgmt | UpdateUser | /auth/users/{username} | PUT | auth:users:{username} | update
 |  |  |  |  |
Authorization | GetVersion | /auth/policies/version | GET | service_info:version | read
Authorization | CreatePolicy | /auth/policies | POST | auth:policies | create
Authorization | ListPolicies | /auth/policies | GET | auth:policies | read
Authorization | DeletePolicy | /auth/policies/{id} | DELETE | auth:policies:{id} | delete
Authorization | IntrospectAll | /auth/introspect | GET | auth_introspection:introspect_all | read
Authorization | IntrospectSome | /auth/introspect_some | POST | auth_introspection:introspect_some | read
Authorization | Introspect | /auth/introspect | POST | auth_introspection:introspect | read
{{% /table %}}
