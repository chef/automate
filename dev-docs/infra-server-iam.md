+++
title = "IAM Actions"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "IAM Actions"
    parent = "automate/authorization"
    identifier = "automate/authorization/iam_actions.md IAM Actions"
    weight = 30
+++


*IAM Action* lists the associated action or actions required to access that page in the browser. 
Use `*` in these actions to give broad permissions to perform all associated actions such as get, list, create, delete, etc.
Specify the action to restrict user access to the specific action.
===

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

{{% responsive-table %}}
|  Task           | Method     | IAM Action       | API endpoint  | URL       |
| --------------- | --------------- | ---------------- | ------------- | --------- |
| List Infra Servers | GET | infra:infraServers:list | /api/v0/infra/servers | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers |
| Get Infra Server | GET | infra:infraServers:get | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
| Create Infra Server | POST | infra:infraServers:create | /api/v0/infra/servers | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers |
| Update Infra Server | PUT | infra:infraServers:update | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
| Delete Infra Server | DELETE | infra:infraServers:delete | /api/v0/infra/servers/{id} | https://{{< example_fqdn "automate" >}}/api/v0/infra/servers/{id} |
{{% /responsive-table %}}
