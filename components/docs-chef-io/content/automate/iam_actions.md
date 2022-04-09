+++
title = "IAM Actions"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "IAM Actions"
    parent = "automate/users/authorization"
    identifier = "automate/users/authorization/iam_actions.md IAM Actions"
    weight = 30
+++

Reference the chart on this page when creating a *Role* to know which action grants access to what page in the browser.

*IAM Action* lists the associated action or actions required to access that page in the browser.
Use `*` in these actions to give broad permissions to perform all associated actions such as get, list, create, delete, etc.
Specify the action to restrict user access to the specific action.

{{% responsive-table %}}
|  Task           | Browser Tab     | IAM Action       | API endpoint  | URL       |
| --------------- | --------------- | ---------------- | ------------- | --------- |
| View Events | Dashboards | event:* | /event_feed | https://{{< example_fqdn "automate" >}}/dashboards/event-feed |
| View and Search Events | Dashboards | [event:*, infra:nodes:list] | /event_feed | https://{{< example_fqdn "automate" >}}/dashboards/event-feed |
| View Service Group Data | Applications | applications:*  | /applications/service-groups | https://{{< example_fqdn "automate" >}}/applications/service-groups |
| View Client Runs | Infrastructure | infra:nodes:*   | /cfgmgmt/nodes | https://{{< example_fqdn "automate" >}}/infrastructure/client-runs |
| View Chef Servers | Infrastructure | infra:infraServers:* | /infra/servers | https://{{< example_fqdn "automate" >}}/infrastructure/chef-servers |
| List Reports | Compliance | compliance:reporting:*  | /compliance/reporting/reports | https://{{< example_fqdn "automate" >}}/compliance/reports/overview |
| List Scan Jobs | Compliance | compliance:scannerJobs:* | /compliance/scanner/jobs | https://{{< example_fqdn "automate" >}}/compliance/scan-jobs/jobs |
| Manage Scan Jobs | Compliance | [compliance:scannerJobs:* , infra:nodes:* , infra:nodeManagers:* , compliance:profiles:* ] | /compliance/scanner/jobs | https://{{< example_fqdn "automate" >}}/compliance/scan-jobs/jobs |
| Manage Compliance Profiles | Compliance | compliance:profiles:* | /compliance/profiles | https://{{< example_fqdn "automate" >}}/compliance/compliance-profiles |
| Manage Notifications | Settings | notifications:* | /notifications | https://{{< example_fqdn "automate" >}}/settings/notifications |
| Manage Data Feed | Settings | datafeed:* | /data_feed/destination | https://{{< example_fqdn "automate" >}}/settings/data-feed |
| Manage Node Integrations | Settings | [infra:nodeManagers:* , infra:nodes:* , secrets:* ] | /nodemanagers , /cfgmgmt/nodes , /secrets | https://{{< example_fqdn "automate" >}}/settings/node-integrations |
| Manage Node Credentials | Settings | secrets:* | /secrets | https://{{< example_fqdn "automate" >}}/settings/node-credentials |
| Manage Data Lifecycle | Settings | dataLifecycle:* | /data-lifecycle | https://{{< example_fqdn "automate" >}}/settings/data-lifecycle |
| Manage Users | Settings | iam:users:* | /iam/v2/users | https://{{< example_fqdn "automate" >}}/settings/users |
| Manage Teams | Settings | iam:teams:* | /iam/v2/teams | https://{{< example_fqdn "automate" >}}/settings/teams |
| Manage API Tokens | Settings | iam:tokens:* | /iam/v2/tokens | https://{{< example_fqdn "automate" >}}/settings/tokens |
| Manage Policies | Settings | iam:policies:* | /iam/v2/policies | https://{{< example_fqdn "automate" >}}/settings/policies |
| Manage Roles | Settings | iam:roles:* | /iam/v2/roles | https://{{< example_fqdn "automate" >}}/settings/roles |
| Manage Projects | Settings | iam:projects:* | /iam/v2/projects | https://{{< example_fqdn "automate" >}}/settings/projects |
{{% /responsive-table %}}

##Infra Server View Actions

These are the *IAM Actions* for the different views and action in the *Infra Server View* tab in Automate.

| Task | Method     | IAM Action | API endpoint  | URL     |
| ------| ---------- | --------- | ------------- | --------- |
| List Infra Servers | GET | infra:infraServers:list | /api/v0/infra/servers | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers |
| Get Infra Server | GET | infra:infraServers:get | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
| Create Infra Server | POST | infra:infraServers:create | /api/v0/infra/servers | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers |
| Update Infra Server | PUT | infra:infraServers:update | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
| Delete Infra Server | DELETE | infra:infraServers:delete | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
| List Orgs | GET | infra:infraServersOrgs:list | /api/v0/infra/servers/{server_id}/orgs | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs |
| Get Org | GET | infra:infraServersOrgs:get | /api/v0/infra/servers/{server_id}/orgs/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{id} |
| Create Org | POST | infra:infraServersOrgs:create,iam:projects:assign | /api/v0/infra/servers/{server_id}/orgs | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs |
| Update Org | PUT | infra:infraServersOrgs:update | /api/v0/infra/servers/{server_id}/orgs/{id}  | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{id} |
| Delete Org | DELETE | infra:infraServersOrgs:delete | /api/v0/infra/servers/{server_id}/orgs/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{id} |
| List Cookbooks | GET | infra:infraServersOrgsCookbooks:list | /api/v0/infra/servers/{server_id}/orgs/{org_id}/cookbooks | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/cookbooks |
| Get Cookbooks | GET | infra:infraServersOrgsCookbooks:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/cookbooks/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/cookbooks/{name} |
| List Roles | GET | infra:infraServersOrgsRoles:list | /api/v0/infra/servers/{id}/orgs/{org_id}/roles | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/roles |
| Get Roles | GET | infra:infraServersOrgsRoles:get | /api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} |
| Create Roles | POST | infra:infraServersOrgsRoles:create | /api/v0/infra/servers/{id}/orgs/{org_id}/roles | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/roles |
| Update Roles | PUT | infra:infraServersOrgsRoles:update | /api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} |
| Delete Roles | DELETE | infra:infraServersOrgsRoles:delete | /api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/roles/{name} |
| List Environments | GET | infra:infraServersOrgsEnvironments:list | /api/v0/infra/servers/{id}/orgs/{org_id}/environments | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/environments |
| Get Environments | GET | infra:infraServersOrgsEnvironments:get | /api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} |
| Create Environments | POST | infra:infraServersOrgsEnvironments:create | /api/v0/infra/servers/{id}/orgs/{org_id}/environments | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/environments |
| Update Environments | PUT | infra:infraServersOrgsEnvironments:update | /api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} |
| Delete Environments | DELETE | infra:infraServersOrgsEnvironments:delete | /api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/environments/{name} |
| Get DataBags | GET | infra:infraServersOrgsDataBags:get | /api/v0/infra/servers/{id}/orgs/{org_id}/data_bags | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/data_bags |
| Create DataBags | POST | infra:infraServersOrgsDataBags:create | /api/v0/infra/servers/{id}/orgs/{org_id}/data_bags | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/data_bags |
| Delete DataBags | DELETE | infra:infraServersOrgsDataBags:delete | /api/v0/infra/servers/{id}/orgs/{org_id}/data_bags/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id}/orgs/{org_id}/data_bags/{name} |
| Get DataBagItem | GET | infra:infraServersOrgsDataBagsItem:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item} |
| Create DataBagItem | POST | infra:infraServersOrgsDataBagsItem:create | /api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name} |
| Update DataBagItem | PUT | infra:infraServersOrgsDataBagsItem:update | /api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item_id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item_id} |
| Delete DataBagsItem | DELETE | infra:infraServersOrgsDataBagsItem:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name}/{item} |
| Delete DataBags | DELETE | infra:infraServersOrgsDataBags:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/data_bags/{name} |
| Get PolicyFiles | GET | infra:infraServersOrgsPolicyFiles:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/policyfiles| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/policyfiles |
| Delete PolicyFiles | DELETE | infra:infraServersOrgsPolicyFiles:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/policyfiles/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/policyfiles/{name} |
| Get PolicyGroups | GET | infra:infraServersOrgsPolicyGroups:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/policygroups/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/policygroups/{name} |
| Get Client | GET | infra:infraServersOrgsClient:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name} |
| Create Client | POST | infra:infraServersOrgsClient:create | /api/v0/infra/servers/{server_id}/orgs/{org_id}/clients| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/clients |
| Reset Client Key | PUT | infra:infraServersOrgsClient:update | /api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name}/reset| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name}/reset |
| Delete Client | DELETE | infra:infraServersOrgsClient:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/clients/{name} |
| Get Node | GET | infra:infraServersOrgsNodes:get | /api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes |
| Update Node | POST | infra:infraServersOrgsNodes:update | /api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes |
| Delete Node | DELETE | infra:infraServersOrgsNodes:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes/{name} |

Three types of user policies automatically gets created with creation of every project

###Infra Viewer Policy Actions

```   
    secrets:*:get,
    secrets:*:list,
    infra:*:get,
    infra:*:list,
    compliance:*:get,
    compliance:*:list,
    event:*:get,
    event:*:list,
    ingest:*:get,
    ingest:*:list,
    iam:projects:list,
    iam:projects:get,
    applications:*:get,
    applications:*:list
```

###Infra Editor Policy Actions

```
    infra:*:list,
    infra:*:get,
    infra:infraServersOrgsRoles:create,
    infra:infraServersOrgsRoles:update,
    infra:infraServersOrgsClient:create,
    infra:infraServersOrgsClient:update,
    infra:infraServersOrgsDataBags:create,
    infra:infraServersOrgsDataBagsItem:create,
    infra:infraServersOrgsDataBagsItem:update,
    infra:infraServersOrgsEnvironments:create,
    infra:infraServersOrgsEnvironments:update,
    infra:infraServersOrgsNodes:update,
    compliance:*,
    event:*,
    ingest:*,
    secrets:*,
    iam:projects:list,
    iam:projects:get,
    iam:projects:assign,
    applications:*
```

###Infra Project Owner Policy Actions

```
    infra:*:list,
    infra:*:get,
    infra:infraServersOrgsRoles:create,
    infra:infraServersOrgsRoles:update,
    infra:infraServersOrgsRoles:delete,
    infra:infraServersOrgsClient:create,
    infra:infraServersOrgsClient:update,
    infra:infraServersOrgsClient:delete,
    infra:infraServersOrgsDataBags:create,
    infra:infraServersOrgsDataBags:delete,
    infra:infraServersOrgsDataBagsItem:create,
    infra:infraServersOrgsDataBagsItem:update,
    infra:infraServersOrgsDataBagsItem:delete,
    infra:infraServersOrgsEnvironments:create,
    infra:infraServersOrgsEnvironments:update,
    infra:infraServersOrgsEnvironments:delete,
    infra:infraServersOrgsNodes:update,
    infra:infraServersOrgsNodes:delete,
    infra:infraServersOrgsPolicyFiles:delete,
    compliance:*,
    event:*,
    ingest:*,
    secrets:*,
    iam:projects:list,
    iam:projects:get,
    iam:projects:assign,
    iam:policies:list,
    iam:policies:get,
    iam:policyMembers:*,
    iam:teams:list,
    iam:teams:get,
    iam:teamUsers:*,
    iam:users:get,
    iam:users:list,
    applications:*
```