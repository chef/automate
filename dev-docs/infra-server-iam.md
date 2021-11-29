*IAM Action* lists the associated action or actions required to access page in the browser.
Use `*` in these actions to give broad permissions to perform all associated actions such as get, list, create, delete, etc
Specify the action to restrict user access to the specific action

### How to assign IAM Actions to user/teams/api-token

 *  Make a `post` request to `https://{{< example_fqdn "automate" >}}/apis/iam/v2/policies`
    with body as
    ```
    {
    "name": "username",
    "id": "policyname",
    "projects": [],
    "members": [
        "user:local:username",
        "user:ldap:username",
        "user:saml:username",
        "team:local:teamname",
        "token:id"
    ],
    "statements": [
        {
            "effect": "ALLOW",
            "actions": [
                "infra:infraServers:list",
                "infra:nodes:list"
            ],
            "projects": ["*"]
        }
    ]
    }
    ```

- Note: members array may have single item like `"user:local:username"` to add specific actions to only one user also actions array mentioned above are minimal set of actions required to view InfraServer tab.



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
| Delete PolicyFiles | DELETE | infra:infraServersOrgsNodes:delete | /api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes/{name}| https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{server_id}/orgs/{org_id}/nodes/{name} |
